package owe.entities.active.behaviour.walker

import akka.actor.Actor.Receive
import akka.pattern.ask
import owe.EntityID
import owe.entities.ActiveEntity.{ActiveEntityData, ForwardMessage, ProcessEntityTick, WalkerData}
import owe.entities.active.Walker._
import owe.entities.active._
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.entities.active.behaviour.walker.transformations.{
  ProcessedMovement,
  ProcessedPath,
  ProcessedUpdateMessages,
  RoamAction
}
import owe.entities.active.behaviour.{BaseBehaviour, UpdateExchange}
import owe.entities.passive.{Doodad, Road, Roadblock}
import owe.map.GameMap._
import owe.map.grid.Point
import owe.map.{ActiveMapEntity, MapEntity, PassiveMapEntity}
import owe.production.{Commodity, CommodityAmount, CommodityState}

import scala.collection.immutable.Queue
import scala.concurrent.Future

trait BaseWalker
    extends BaseBehaviour[Walker.ActorRefTag]
    with ProcessedUpdateMessages
    with ProcessedMovement
    with ProcessedPath
    with RoamAction {

  import context.dispatcher

  final protected def attacking(nextBehaviour: () => Behaviour): Behaviour = {
    case ProcessEntityTick(_, entity: WalkerData, messages) =>
      nextEnemyEntity(entity)
        .foreach {
          case Some((enemyEntityId, damage)) =>
            attackEntity(enemyEntityId, damage)
            withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
              .foreach { updatedData =>
                self ! Become(() => attacking(nextBehaviour), updatedData)
              }

          case None =>
            withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
              .foreach { updatedData =>
                self ! Become(nextBehaviour, updatedData)
              }
        }
  }

  final protected def idling(): Behaviour = {
    case ProcessEntityTick(_, entity: WalkerData, messages) =>
      nextEnemyEntity(entity)
        .foreach {
          case Some(_) =>
            withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
              .foreach { updatedData =>
                self ! Become(() => attacking(idling), updatedData)
              }

          case None =>
            withAsyncUpdates(
              entity,
              Seq(
                withProcessedUpdateMessages(_: WalkerData, messages),
                withMovementMode(_: WalkerData, MovementMode.Idling)
              )
            ).foreach { updatedData =>
              self ! Become(idling, updatedData)
            }
        }
  }

  final protected def roaming(roamAction: Action): Behaviour = {
    case ProcessEntityTick(_, entity: WalkerData, messages) =>
      nextEnemyEntity(entity)
        .foreach {
          case Some(_) =>
            withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
              .foreach { updatedData =>
                self ! Become(() => attacking(() => roaming(roamAction)), updatedData)
              }

          case None =>
            val maxDistance = entity.modifiers.maxRoamingDistance(entity.properties.maxRoamingDistance)
            val distanceCovered = entity.state.distanceCovered

            val canRoam = roamAction match {
              case DoRepeatableOperation(_, repeat) => repeat(entity)
              case _                                => true
            }

            if (canRoam && maxDistance > distanceCovered) {
              nextPosition(entity)
                .foreach { nextPositionOpt =>
                  val updates: Future[Seq[WalkerData => Future[State]]] = nextPositionOpt match {
                    case Some((nextPosition, remainingPath)) =>
                      moveEntity(entity.properties.id, nextPosition)
                      Future.successful(
                        Seq(
                          withProcessedUpdateMessages(_, messages),
                          withRoamAction(_, roamAction),
                          withProcessedMovement,
                          withProcessedPath(_, remainingPath),
                          withMovementMode(_, MovementMode.Roaming)
                        )
                      )

                    case None =>
                      //no free path; can't move
                      calculateRoamingPath(entity.properties.id, maxDistance).map {
                        case Some(newRoamingPath) =>
                          Seq(
                            withProcessedUpdateMessages(_, messages),
                            withRoamAction(_, roamAction),
                            withProcessedPath(_, newRoamingPath),
                            withMovementMode(_, MovementMode.Idling)
                          )

                        case None =>
                          log.error("Failed to generate new roaming path for entity [{}].", entity.properties.id)
                          Seq.empty
                      }
                  }

                  updates.foreach { updates =>
                    withAsyncUpdates(entity, updates)
                      .foreach { updatedData =>
                        self ! Become(() => roaming(roamAction), updatedData)
                      }
                  }
                }
            } else {
              withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
                .foreach { updatedData =>
                  self ! Become(
                    () => advancing(MovementMode.Returning, destination = Home, destinationActions = Seq.empty),
                    updatedData
                  )
                }
            }
        }
  }

  final protected def advancing(
    mode: MovementMode,
    destination: Destination,
    destinationActions: Seq[Action]
  ): Behaviour = {
    case ProcessEntityTick(map, entity: WalkerData, messages) =>
      val actualDestination: Future[Point] = destination match {
        case DestinationPoint(point)     => Future.successful(point)
        case DestinationEntity(entityID) => getEntityData(entityID).map(_.properties.homePosition)
        case Home                        => Future.successful(entity.properties.homePosition)
      }

      actualDestination
        .foreach { actualDestination =>
          if (map.position == actualDestination) {
            withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
              .foreach { updatedData =>
                self ! Become(() => acting(destinationActions), updatedData)
              }
          } else {
            nextPosition(entity).foreach { nextPositionOpt =>
              val updates: Future[Seq[WalkerData => Future[State]]] = nextPositionOpt match {
                case Some((nextPosition, remainingPath)) =>
                  moveEntity(entity.properties.id, nextPosition)
                  Future.successful(
                    Seq(
                      withProcessedUpdateMessages(_, messages),
                      withProcessedMovement,
                      withProcessedPath(_, remainingPath),
                      withMovementMode(_, mode)
                    )
                  )

                case None =>
                  //no free path; can't move
                  calculateAdvancePath(entity.properties.id, actualDestination).map {
                    case Some(newAdvancePath) =>
                      Seq(
                        withProcessedUpdateMessages(_, messages),
                        withProcessedPath(_, newAdvancePath),
                        withMovementMode(_, MovementMode.Idling)
                      )

                    case None =>
                      log.error("Failed to generate new advance path for entity [{}].", entity.properties.id)
                      Seq.empty
                  }
              }

              updates.foreach { updates =>
                withAsyncUpdates(entity, updates)
                  .foreach { updatedData =>
                    self ! Become(() => advancing(mode, destination, destinationActions), updatedData)
                  }
              }
            }
          }
        }
  }

  final protected def acting(actions: Seq[Action]): Behaviour = {
    case ProcessEntityTick(_, entity: WalkerData, messages) =>
      val (updates, behaviour) = actions match {
        case currentAction :: remainingActions =>
          currentAction match {
            case DoOperation(op) =>
              (
                Seq(withProcessedUpdateMessages(_: WalkerData, messages), op),
                () => acting(remainingActions)
              )

            case DoRepeatableOperation(op, repeat) =>
              (
                Seq(withProcessedUpdateMessages(_: WalkerData, messages), op),
                if (repeat(entity))() => acting(actions)
                else () => acting(remainingActions)
              )

            case GoToPoint(destination) =>
              (
                Seq(withProcessedUpdateMessages(_: WalkerData, messages)),
                () => advancing(MovementMode.Advancing, DestinationPoint(destination), remainingActions)
              )

            case GoToEntity(entityID) =>
              (
                Seq(withProcessedUpdateMessages(_: WalkerData, messages)),
                () => advancing(MovementMode.Advancing, DestinationEntity(entityID), remainingActions)
              )

            case GoHome() =>
              (
                Seq(withProcessedUpdateMessages(_: WalkerData, messages)),
                () => advancing(MovementMode.Returning, Home, remainingActions)
              )

            case transition: Transition =>
              if (remainingActions.nonEmpty) {
                log.error(
                  "Transition [{}] will be executed; remaining actions are unreachable: [{}].",
                  transition,
                  remainingActions.mkString(", ")
                )
              }

              transition match {
                case Roam(roamAction) =>
                  (
                    Seq(withProcessedUpdateMessages(_: WalkerData, messages)),
                    () => roaming(roamAction)
                  )

                case Idle() =>
                  (
                    Seq(withProcessedUpdateMessages(_: WalkerData, messages)),
                    () => idling()
                  )
              }

            case NoAction =>
              (
                Seq(withProcessedUpdateMessages(_: WalkerData, messages)),
                () => acting(remainingActions)
              )
          }

        case _ =>
          (
            Seq(withProcessedUpdateMessages(_: WalkerData, messages)),
            destroying _
          )
      }

      withAsyncUpdates(entity, updates)
        .foreach(updatedData => self ! Become(behaviour, updatedData))
  }

  protected def getEntityData(entityID: EntityID): Future[ActiveEntityData] =
    (parentEntity ? ForwardMessage(GetEntity(entityID))).mapTo[ActiveEntityData]

  protected def getNeighboursData(
    walkerId: EntityID,
    radius: Distance
  ): Future[Seq[(EntityID, ActiveEntityData)]] =
    (parentEntity ? ForwardMessage(GetNeighbours(walkerId, radius))).mapTo[Seq[(EntityID, ActiveEntityData)]]

  protected def distributeCommodities(
    entityID: EntityID,
    commodities: Seq[(Commodity, CommodityAmount)]
  ): Unit =
    parentEntity ! ForwardMessage(DistributeCommodities(entityID, commodities))

  protected def destroying(): Behaviour = {
    case ProcessEntityTick(_, entity, _) =>
      log.debug("Entity [{}] waiting to be destroyed; tick ignored", self)
      sender() ! entity.state
  }

  private def base(): Behaviour = {
    case Become(behaviour, walker) =>
      parentEntity ! walker.state
      become(behaviour, walker)
  }

  private def become(behaviour: () => Behaviour, walker: WalkerData): Unit =
    if (walker.state.currentLife.isSufficient) {
      context.become(base().orElse(behaviour()))
    } else {
      walker.state.commodities match {
        case CommoditiesState(available, _) => UpdateExchange.State(available, CommodityState.Lost)
        case _                              => //do nothing
      }

      parentEntity ! ForwardMessage(DestroyEntity(walker.properties.id))
      context.become(destroying())
    }

  private def attackEntity(id: EntityID, damage: AttackDamage): Unit =
    parentEntity ! ForwardMessage(AttackEntity(id, damage))

  private def moveEntity(id: EntityID, destination: Point): Unit =
    parentEntity ! ForwardMessage(MoveEntity(id, destination))

  private def nextEnemyEntity(walker: WalkerData): Future[Option[(EntityID, AttackDamage)]] =
    (walker.properties.attack, walker.modifiers.attack) match {
      case (attackProperties: AttackProperties, attackModifiers: AttackModifiers) =>
        getNeighboursData(walker.properties.id, attackModifiers.distance(attackProperties.distance))
          .map { neighbours =>
            neighbours.collectFirst {
              case (entityID, entity) if attackProperties.target(entity) =>
                (entityID, attackModifiers.damage(attackProperties.damage))
            }
          }

      case _ => Future.successful(None)
    }

  private def getEntitiesData(point: Point): Future[Seq[(MapEntity, Option[ActiveEntityData])]] =
    (parentEntity ? ForwardMessage(GetEntities(point))).mapTo[Seq[(MapEntity, Option[ActiveEntityData])]]

  private def calculateAdvancePath(walkerId: EntityID, destination: Point): Future[Option[Queue[Point]]] =
    (parentEntity ? ForwardMessage(GetAdvancePath(walkerId, destination))).mapTo[Option[Queue[Point]]]

  private def calculateRoamingPath(walkerId: EntityID, length: Distance): Future[Option[Queue[Point]]] =
    (parentEntity ? ForwardMessage(GetRoamingPath(walkerId, length))).mapTo[Option[Queue[Point]]]

  private def nextPosition(walker: WalkerData): Future[Option[(Point, Queue[Point])]] = {
    val cellsTravelled = cellsPerTick(walker)
    val currentPath = walker.state.path
    val travelPath = currentPath.take(cellsTravelled)

    Future.sequence(travelPath.map(getEntitiesData)).map { entities =>
      val isTravelPathPassable = entities.flatten.forall {
        case (mapEntity, entityData) => isPassable(mapEntity, entityData, walker)
      }

      if (isTravelPathPassable) {
        if (cellsTravelled >= currentPath.size) {
          currentPath.lastOption.map((_, Queue.empty))
        } else {
          currentPath.drop(cellsTravelled - 1).dequeueOption
        }
      } else {
        None
      }
    }
  }

  private def cellsPerTick(walker: WalkerData): Int =
    walker.modifiers.movementSpeed(walker.properties.movementSpeed).value

  private def isPassable(mapEntity: MapEntity, entityData: Option[ActiveEntityData], walker: WalkerData): Boolean =
    mapEntity match {
      case mapEntity: PassiveMapEntity =>
        mapEntity.entity match {
          case _: Doodad    => false
          case _: Road      => true
          case _: Roadblock => walker.state.mode == MovementMode.Roaming
        }

      case mapEntity: ActiveMapEntity =>
        mapEntity.entity match {
          case _: Structure.ActorRefTag => false

          case _: Resource.ActorRefTag => false

          case _: Walker.ActorRefTag =>
            walker.properties.attack match {
              case props: AttackProperties => !entityData.forall(props.target)
              case _                       => false
            }
        }
    }
}

object BaseWalker {
  sealed trait Action
  case object NoAction extends Action
  case class DoOperation(op: WalkerData => Future[State]) extends Action
  case class DoRepeatableOperation(op: WalkerData => Future[State], repeat: WalkerData => Boolean) extends Action
  case class GoToPoint(destination: Point) extends Action
  case class GoToEntity(entityID: EntityID) extends Action
  case class GoHome() extends Action

  sealed trait Transition extends Action
  case class Roam(roamAction: Action) extends Transition
  case class Idle() extends Transition

  sealed trait Destination
  case object Home extends Destination
  case class DestinationPoint(point: Point) extends Destination
  case class DestinationEntity(entityID: EntityID) extends Destination

  private[behaviour] case class Become(behaviour: () => Receive, walker: WalkerData)
}
