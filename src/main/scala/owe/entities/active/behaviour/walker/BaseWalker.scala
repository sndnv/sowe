package owe.entities.active.behaviour.walker

import akka.actor.Actor.Receive
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity._
import owe.entities.ActiveEntityActor._
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker._
import owe.entities.active.attributes.{AttackDamage, Distance}
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.entities.active.behaviour.walker.transformations.{
  ProcessedMovement,
  ProcessedPath,
  ProcessedUpdateMessages,
  RoamAction
}
import owe.entities.active.behaviour.{BaseBehaviour, UpdateExchange}
import owe.entities.passive.Doodad.DoodadRef
import owe.entities.passive.Road.RoadRef
import owe.entities.passive.Roadblock.RoadblockRef
import owe.map.GameMap._
import owe.map.MapEntity
import owe.map.grid.Point
import owe.production.Commodity

import scala.collection.immutable.Queue
import scala.concurrent.Future

trait BaseWalker
    extends BaseBehaviour
    with ProcessedUpdateMessages
    with ProcessedMovement
    with ProcessedPath
    with RoamAction {

  import context.dispatcher

  final override private[behaviour] implicit val parentEntity: ActiveEntity.ActiveEntityRef =
    WalkerRef(context.parent)

  final protected def attacking(nextBehaviour: () => Behaviour): Behaviour = {
    case ApplyMessages(walker: WalkerData, messages) =>
      log.debug("Applying [{}] messages: [{}]", messages.size, messages)

      withAsyncUpdates(
        walker,
        Seq(
          withProcessedUpdateMessages(_: WalkerData, messages)
        )
      ).foreach { updatedData =>
        parentEntity ! MessagesApplied(updatedData.state)
      }

    case ProcessBehaviourTick(_, walker: WalkerData) =>
      log.debug("Processing behaviour tick as [attacking] with data [{}]", walker)

      nextEnemyEntity(walker)
        .foreach {
          case Some((enemyEntityId, damage)) =>
            attackEntity(enemyEntityId, damage)
            self ! Become(() => attacking(nextBehaviour), walker)

          case None =>
            self ! Become(nextBehaviour, walker)
        }
  }

  final protected def idling(): Behaviour = {
    case ApplyMessages(walker: WalkerData, messages) =>
      log.debug("Applying [{}] messages: [{}]", messages.size, messages)

      withAsyncUpdates(
        walker,
        Seq(
          withProcessedUpdateMessages(_: WalkerData, messages)
        )
      ).foreach { updatedData =>
        parentEntity ! MessagesApplied(updatedData.state)
      }

    case ProcessBehaviourTick(_, walker: WalkerData) =>
      log.debug("Processing behaviour tick as [idling] with data [{}]", walker)

      nextEnemyEntity(walker)
        .foreach {
          case Some(_) =>
            self ! Become(() => attacking(() => idling()), walker)

          case None =>
            withAsyncUpdates(
              walker,
              Seq(withMovementMode(_: WalkerData, MovementMode.Idling))
            ).foreach { updatedData =>
              self ! Become(() => idling(), updatedData)
            }
        }
  }

  final protected def roaming(roamAction: Action): Behaviour = {
    case ApplyMessages(walker: WalkerData, messages) =>
      log.debug("Applying [{}] messages: [{}]", messages.size, messages)

      withAsyncUpdates(
        walker,
        Seq(
          withProcessedUpdateMessages(_: WalkerData, messages)
        )
      ).foreach { updatedData =>
        parentEntity ! MessagesApplied(updatedData.state)
      }

    case ProcessBehaviourTick(_, walker: WalkerData) =>
      log.debug("Processing behaviour tick as [roaming({})] with data [{}]", roamAction, walker)

      nextEnemyEntity(walker)
        .foreach {
          case Some(_) =>
            self ! Become(() => attacking(() => roaming(roamAction)), walker)

          case None =>
            val maxDistance = walker.modifiers.maxRoamingDistance(walker.properties.maxRoamingDistance)
            val distanceCovered = walker.state.distanceCovered

            val canRoam = roamAction match {
              case DoRepeatableOperation(_, repeat) => repeat(walker)
              case _                                => true
            }

            if (canRoam && maxDistance > distanceCovered) {
              nextPosition(walker)
                .foreach { nextPositionOpt =>
                  val updates: Future[Seq[WalkerData => Future[State]]] = nextPositionOpt match {
                    case Some((nextPosition, remainingPath)) =>
                      moveEntity(walker.id, nextPosition)
                      Future.successful(
                        Seq(
                          withRoamAction(_, roamAction),
                          withProcessedMovement,
                          withProcessedPath(_, remainingPath),
                          withMovementMode(_, MovementMode.Roaming)
                        )
                      )

                    case None =>
                      //no free path; can't move
                      calculateRoamingPath(walker.id, maxDistance - distanceCovered).map { newRoamingPath =>
                        if (newRoamingPath.nonEmpty) {
                          Seq(
                            withRoamAction(_, roamAction),
                            withProcessedPath(_, newRoamingPath),
                            withMovementMode(_, MovementMode.Roaming)
                          )
                        } else {
                          log.error("Failed to generate new roaming path for walker [{}]", walker.id)
                          Seq(
                            withMovementMode(_, MovementMode.Idling)
                          )
                        }
                      }
                  }

                  updates.foreach { updates =>
                    withAsyncUpdates(walker, updates)
                      .foreach { updatedData =>
                        self ! Become(() => roaming(roamAction), updatedData)
                      }
                  }
                }
            } else {
              self ! Become(
                () => advancing(MovementMode.Returning, destination = Home, destinationActions = Seq.empty),
                walker
              )
            }
        }
  }

  final protected def advancing(
    mode: MovementMode,
    destination: Destination,
    destinationActions: Seq[Action]
  ): Behaviour = {
    case ApplyMessages(walker: WalkerData, messages) =>
      log.debug("Applying [{}] messages: [{}]", messages.size, messages)

      withAsyncUpdates(
        walker,
        Seq(
          withProcessedUpdateMessages(_: WalkerData, messages)
        )
      ).foreach { updatedData =>
        parentEntity ! MessagesApplied(updatedData.state)
      }

    case ProcessBehaviourTick(map, walker: WalkerData) =>
      log.debug(
        "Processing behaviour tick as [advancing({}, {}, {})] with data [{}]",
        mode,
        destination,
        destinationActions,
        walker
      )

      val actualDestination: Future[Point] = destination match {
        case DestinationPoint(point)     => Future.successful(point)
        case DestinationEntity(entityID) => getEntityData(entityID).map(_.properties.homePosition)
        case Home                        => Future.successful(walker.properties.homePosition)
      }

      actualDestination
        .foreach { actualDestination =>
          if (map.position == actualDestination) {
            self ! Become(() => acting(destinationActions), walker)
          } else {
            nextPosition(walker).foreach { nextPositionOpt =>
              val updates: Future[Seq[WalkerData => Future[State]]] = nextPositionOpt match {
                case Some((nextPosition, remainingPath)) =>
                  moveEntity(walker.id, nextPosition)
                  Future.successful(
                    Seq(
                      withProcessedMovement,
                      withProcessedPath(_, remainingPath),
                      withMovementMode(_, mode)
                    )
                  )

                case None =>
                  //no free path; can't move
                  calculateAdvancePath(walker.id, actualDestination).map { newAdvancePath =>
                    if (newAdvancePath.nonEmpty) {
                      Seq(
                        withProcessedPath(_, newAdvancePath),
                        withMovementMode(_, mode)
                      )
                    } else {
                      log.error("Failed to generate new advance path for walker [{}]", walker.id)
                      Seq(
                        withMovementMode(_, MovementMode.Idling)
                      )
                    }
                  }
              }

              updates.foreach { updates =>
                withAsyncUpdates(walker, updates)
                  .foreach { updatedData =>
                    self ! Become(() => advancing(mode, destination, destinationActions), updatedData)
                  }
              }
            }
          }
        }
  }

  final protected def acting(actions: Seq[Action]): Behaviour = {
    case ApplyMessages(walker: WalkerData, messages) =>
      log.debug("Applying [{}] messages: [{}]", messages.size, messages)

      withAsyncUpdates(
        walker,
        Seq(
          withProcessedUpdateMessages(_: WalkerData, messages)
        )
      ).foreach { updatedData =>
        parentEntity ! MessagesApplied(updatedData.state)
      }

    case ProcessBehaviourTick(_, walker: WalkerData) =>
      log.debug("Processing behaviour tick as [acting({})] with data [{}]", actions, walker)

      actions match {
        case currentAction :: remainingActions =>
          currentAction match {
            case DoOperation(op) =>
              withAsyncUpdates(walker, Seq(op))
                .foreach(updatedData => self ! Become(() => acting(remainingActions), updatedData))

            case DoRepeatableOperation(op, repeat) =>
              if (repeat(walker)) {
                withAsyncUpdates(walker, Seq(op))
                  .foreach(updatedData => self ! Become(() => acting(actions), updatedData))
              } else {
                self ! Become(() => acting(remainingActions), walker)
              }

            case GoToPoint(destination) =>
              self ! Become(
                () => advancing(MovementMode.Advancing, DestinationPoint(destination), remainingActions),
                walker
              )

            case GoToEntity(entityID) =>
              self ! Become(
                () => advancing(MovementMode.Advancing, DestinationEntity(entityID), remainingActions),
                walker
              )

            case GoHome() =>
              self ! Become(() => advancing(MovementMode.Returning, Home, remainingActions), walker)

            case transition: Transition =>
              if (remainingActions.nonEmpty) {
                log.error(
                  "Transition [{}] will be executed; remaining actions are unreachable: [{}]",
                  transition,
                  remainingActions.mkString(", ")
                )
              }

              transition match {
                case Roam(roamAction) =>
                  self ! Become(() => roaming(roamAction), walker)

                case Idle() =>
                  self ! Become(() => idling(), walker)
              }

            case NoAction =>
              self ! Become(() => acting(remainingActions), walker)
          }

        case _ =>
          walker.state.commodities match {
            case CommoditiesState(available, _) => UpdateExchange.State(available, Commodity.State.Lost)
            case _                              => //do nothing
          }

          parentEntity ! ForwardMessage(DestroyEntity(walker.id))

          self ! Become(() => destroying(), walker)
      }
  }

  protected def getEntityData(entityID: ActiveEntityRef): Future[Data] =
    (parentEntity ? ForwardMessage(GetEntity(entityID))).mapTo[Data]

  protected def getNeighboursData(
    walkerId: WalkerRef,
    radius: Distance
  ): Future[Seq[(ActiveEntityRef, Data)]] =
    (parentEntity ? ForwardMessage(GetNeighbours(walkerId, radius))).mapTo[Seq[(ActiveEntityRef, Data)]]

  protected def distributeCommodities(
    entityID: ActiveEntityRef,
    commodities: Seq[(Commodity, Commodity.Amount)]
  ): Unit =
    parentEntity ! ForwardMessage(DistributeCommodities(entityID, commodities))

  protected def destroying(): Behaviour = {
    case ApplyMessages(walker: WalkerData, _) =>
      log.debug("Entity [{}] waiting to be destroyed; messages application ignored", self)
      parentEntity ! MessagesApplied(walker.state)

    case ProcessBehaviourTick(_, entity) =>
      log.debug("Entity [{}] waiting to be destroyed; tick ignored", self)
      parentEntity ! BehaviourTickProcessed(entity.state)
  }

  override protected def base: Behaviour = {
    case Become(behaviour, walker) =>
      parentEntity ! BehaviourTickProcessed(walker.state)
      become(behaviour, walker)
  }

  private def become(behaviour: () => Behaviour, walker: WalkerData): Unit =
    if (walker.state.currentLife.isSufficient) {
      context.become(base.orElse(behaviour()))
    } else {
      walker.state.commodities match {
        case CommoditiesState(available, _) => UpdateExchange.State(available, Commodity.State.Lost)
        case _                              => //do nothing
      }

      parentEntity ! ForwardMessage(DestroyEntity(walker.id))
      context.become(destroying())
    }

  private def attackEntity(id: ActiveEntityRef, damage: AttackDamage): Unit =
    parentEntity ! ForwardMessage(AttackEntity(id, damage))

  private def moveEntity(id: ActiveEntityRef, destination: Point): Unit =
    parentEntity ! ForwardMessage(MoveEntity(id, destination))

  private def nextEnemyEntity(walker: WalkerData): Future[Option[(ActiveEntityRef, AttackDamage)]] =
    (walker.properties.attack, walker.modifiers.attack) match {
      case (attackProperties: AttackProperties, attackModifiers: AttackModifiers) =>
        getNeighboursData(walker.id, attackModifiers.distance(attackProperties.distance))
          .map { neighbours =>
            neighbours.collectFirst {
              case (entityID, entity) if attackProperties.target(entity) =>
                (entityID, attackModifiers.damage(attackProperties.damage))
            }
          }

      case _ => Future.successful(None)
    }

  private def getEntitiesData(point: Point): Future[Seq[(MapEntity, Option[Data])]] =
    (parentEntity ? ForwardMessage(GetEntities(point))).mapTo[Seq[(MapEntity, Option[Data])]]

  private def calculateAdvancePath(walkerId: WalkerRef, destination: Point): Future[Queue[Point]] =
    (parentEntity ? ForwardMessage(GetAdvancePath(walkerId, destination))).mapTo[Queue[Point]]

  private def calculateRoamingPath(walkerId: WalkerRef, length: Distance): Future[Queue[Point]] =
    (parentEntity ? ForwardMessage(GetRoamingPath(walkerId, length))).mapTo[Queue[Point]]

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

  private def isPassable(mapEntity: MapEntity, entityData: Option[Data], walker: WalkerData): Boolean =
    mapEntity.entityRef match {
      case _: DoodadRef    => false
      case _: RoadRef      => true
      case _: RoadblockRef => walker.state.mode != MovementMode.Roaming
      case _: StructureRef => false
      case _: ResourceRef  => false
      case walkerRef: WalkerRef =>
        if (walkerRef != parentEntity) {
          walker.properties.attack match {
            case props: AttackProperties => !entityData.exists(props.target)
            case _                       => true
          }
        } else {
          true
        }
    }

}

object BaseWalker {
  sealed trait Action
  case object NoAction extends Action
  case class DoOperation(op: WalkerData => Future[State]) extends Action
  case class DoRepeatableOperation(op: WalkerData => Future[State], repeat: WalkerData => Boolean) extends Action
  case class GoToPoint(destination: Point) extends Action
  case class GoToEntity(entityID: ActiveEntityRef) extends Action
  case class GoHome() extends Action

  sealed trait Transition extends Action
  case class Roam(roamAction: Action) extends Transition
  case class Idle() extends Transition

  sealed trait Destination
  case object Home extends Destination
  case class DestinationPoint(point: Point) extends Destination
  case class DestinationEntity(entityID: ActiveEntityRef) extends Destination

  private[behaviour] case class Become(behaviour: () => Receive, walker: WalkerData)
}
