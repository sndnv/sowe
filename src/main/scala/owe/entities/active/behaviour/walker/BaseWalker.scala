package owe.entities.active.behaviour.walker

import owe.entities.ActiveEntity.{ActiveEntityActorRef => _, _}
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
import owe.map.GameMap._
import owe.map.MapEntity
import owe.map.grid.Point
import owe.production.{Commodity, CommodityAmount, CommodityState}
import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.AskPattern._
import owe.entities.ActiveEntity
import owe.entities.Entity.EntityActorRef
import owe.entities.active.Resource.ResourceActorRef
import owe.entities.active.Structure.StructureActorRef
import owe.entities.passive.Doodad.DoodadActorRef
import owe.entities.passive.Road.RoadActorRef
import owe.entities.passive.Roadblock.RoadblockActorRef

trait BaseWalker
    extends BaseBehaviour
    with ProcessedUpdateMessages
    with ProcessedMovement
    with ProcessedPath
    with RoamAction {

  final protected def attacking(nextBehaviour: () => Behaviour): Behaviour = Behaviors.receive { (ctx, msg) =>
    import ctx.executionContext

    msg match {
      case ProcessEntityTick(_, entity: WalkerData, messages) =>
        nextEnemyEntity(entity)
          .foreach {
            case Some((enemyEntityId, damage)) =>
              attackEntity(enemyEntityId, damage)
              withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
                .foreach { updatedData =>
                  ctx.self ! Become(() => attacking(nextBehaviour), updatedData)
                }

            case None =>
              withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
                .foreach { updatedData =>
                  ctx.self ! Become(nextBehaviour, updatedData)
                }
          }
    }

    Behaviors.same // TODO
  }

  final protected def idling(): Behaviour = Behaviors.receive { (ctx, msg) =>
    import ctx.executionContext

    msg match {
      case ProcessEntityTick(_, entity: WalkerData, messages) =>
        nextEnemyEntity(entity)
          .foreach {
            case Some(_) =>
              withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
                .foreach { updatedData =>
                  ctx.self ! Become(() => attacking(() => idling()), updatedData)
                }

            case None =>
              withAsyncUpdates(
                entity,
                Seq(
                  withProcessedUpdateMessages(_: WalkerData, messages),
                  withMovementMode(_: WalkerData, MovementMode.Idling)
                )
              ).foreach { updatedData =>
                ctx.self ! Become(() => idling(), updatedData)
              }
          }
    }

    Behaviors.same // TODO
  }

  final protected def roaming(roamAction: Action): Behaviour = Behaviors.receive { (ctx, msg) =>
    import ctx.executionContext

    msg match {
      case ProcessEntityTick(_, entity: WalkerData, messages) =>
        nextEnemyEntity(entity)
          .foreach {
            case Some(_) =>
              withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
                .foreach { updatedData =>
                  ctx.self ! Become(() => attacking(() => roaming(roamAction)), updatedData)
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
                        moveEntity(entity.id, nextPosition)
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
                        calculateRoamingPath(entity.id, maxDistance).map {
                          case Some(newRoamingPath) =>
                            Seq(
                              withProcessedUpdateMessages(_, messages),
                              withRoamAction(_, roamAction),
                              withProcessedPath(_, newRoamingPath),
                              withMovementMode(_, MovementMode.Idling)
                            )

                          case None =>
                            ctx.log.error("Failed to generate new roaming path for entity [{}].", entity.id)
                            Seq.empty
                        }
                    }

                    updates.foreach { updates =>
                      withAsyncUpdates(entity, updates)
                        .foreach { updatedData =>
                          ctx.self ! Become(() => roaming(roamAction), updatedData)
                        }
                    }
                  }
              } else {
                withAsyncUpdates(entity, Seq(withProcessedUpdateMessages(_: WalkerData, messages)))
                  .foreach { updatedData =>
                    ctx.self ! Become(
                      () => advancing(MovementMode.Returning, destination = Home, destinationActions = Seq.empty),
                      updatedData
                    )
                  }
              }
          }
    }

    Behaviors.same // TODO
  }

  final protected def advancing(
    mode: MovementMode,
    destination: Destination,
    destinationActions: Seq[Action]
  ): Behaviour = Behaviors.receive { (ctx, msg) =>
    import ctx.executionContext

    msg match {
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
                  ctx.self ! Become(() => acting(destinationActions), updatedData)
                }
            } else {
              nextPosition(entity).foreach { nextPositionOpt =>
                val updates: Future[Seq[WalkerData => Future[State]]] = nextPositionOpt match {
                  case Some((nextPosition, remainingPath)) =>
                    moveEntity(entity.id, nextPosition)
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
                    calculateAdvancePath(entity.id, actualDestination).map {
                      case Some(newAdvancePath) =>
                        Seq(
                          withProcessedUpdateMessages(_, messages),
                          withProcessedPath(_, newAdvancePath),
                          withMovementMode(_, MovementMode.Idling)
                        )

                      case None =>
                        ctx.log.error("Failed to generate new advance path for entity [{}].", entity.id)
                        Seq.empty
                    }
                }

                updates.foreach { updates =>
                  withAsyncUpdates(entity, updates)
                    .foreach { updatedData =>
                      ctx.self ! Become(() => advancing(mode, destination, destinationActions), updatedData)
                    }
                }
              }
            }
          }
    }

    Behaviors.same // TODO
  }

  final protected def acting(actions: Seq[Action]): Behaviour = Behaviors.receive { (ctx, msg) =>
    import ctx.executionContext

    msg match {
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
                  ctx.log.error(
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
          .foreach(updatedData => ctx.self ! Become(behaviour, updatedData))
    }

    Behaviors.same // TODO
  }

  protected def getEntityData(
    entityID: EntityActorRef
  ): Future[ActiveEntityData] =
    (parentEntity ? ((_: ActorRef[EntityMessage]) => ForwardMessage(GetEntity(entityID)))).mapTo[ActiveEntityData]

  protected def getNeighboursData(
    walkerId: WalkerActorRef,
    radius: Distance
  ): Future[Seq[(EntityActorRef, ActiveEntityData)]] =
    (parentEntity ? ((_: ActorRef[EntityMessage]) => ForwardMessage(GetNeighbours(walkerId, radius))))
      .mapTo[Seq[(EntityActorRef, ActiveEntityData)]]

  protected def distributeCommodities(
    entityID: EntityActorRef,
    commodities: Seq[(Commodity, CommodityAmount)]
  ): Unit =
    parentEntity ! ForwardMessage(DistributeCommodities(entityID, commodities))

  override protected def base: Behaviour = Behaviors.receive { (_, msg) =>
    msg match {
      case Become(behaviour, walker) =>
        parentEntity ! UpdateState(walker.state)

        if (walker.state.currentLife.isSufficient) {
          behaviour()
        } else {
          walker.state.commodities match {
            case CommoditiesState(available, _) => UpdateExchange.State(available, CommodityState.Lost)
            case _                              => //do nothing
          }

          parentEntity ! ForwardMessage(DestroyEntity(walker.id))
          destroying()
        }
    }
  }

  protected def destroying(): Behaviour = Behaviors.receive { (ctx, msg) =>
    msg match {
      case ProcessEntityTick(_, entity, _) =>
        ctx.log.debug("Entity [{}] waiting to be destroyed; tick ignored", ctx.self)
        parentEntity ! UpdateState(entity.state)
    }

    Behaviors.stopped // TODO
  }

  private def attackEntity(id: EntityActorRef, damage: AttackDamage): Unit =
    parentEntity ! ForwardMessage(AttackEntity(id, damage))

  private def moveEntity(id: EntityActorRef, destination: Point): Unit =
    parentEntity ! ForwardMessage(MoveEntity(id, destination))

  private def nextEnemyEntity(walker: WalkerData)(
    implicit ec: ExecutionContext): Future[Option[(EntityActorRef, AttackDamage)]] =
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

  private def getEntitiesData(point: Point): Future[Seq[(MapEntity, Option[ActiveEntityData])]] =
    (parentEntity ? ((_: ActorRef[EntityMessage]) => ForwardMessage(GetEntities(point))))
      .mapTo[Seq[(MapEntity, Option[ActiveEntityData])]]

  private def calculateAdvancePath(walkerId: WalkerActorRef, destination: Point): Future[Option[Queue[Point]]] =
    (parentEntity ? ((_: ActorRef[EntityMessage]) => ForwardMessage(GetAdvancePath(walkerId, destination))))
      .mapTo[Option[Queue[Point]]]

  private def calculateRoamingPath(walkerId: WalkerActorRef, length: Distance): Future[Option[Queue[Point]]] =
    (parentEntity ? ((_: ActorRef[EntityMessage]) => ForwardMessage(GetRoamingPath(walkerId, length))))
      .mapTo[Option[Queue[Point]]]

  private def nextPosition(walker: WalkerData)(implicit ec: ExecutionContext): Future[Option[(Point, Queue[Point])]] = {
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
    mapEntity.entityRef match {
      case _: DoodadActorRef    => false
      case _: RoadActorRef      => true
      case _: RoadblockActorRef => walker.state.mode == MovementMode.Roaming
      case _: StructureActorRef => false
      case _: ResourceActorRef  => false
      case _: WalkerActorRef =>
        walker.properties.attack match {
          case props: AttackProperties => !entityData.forall(props.target)
          case _                       => false
        }
    }

}

object BaseWalker {
  sealed trait Action
  case object NoAction extends Action
  case class DoOperation(op: WalkerData => Future[State]) extends Action
  case class DoRepeatableOperation(op: WalkerData => Future[State], repeat: WalkerData => Boolean) extends Action
  case class GoToPoint(destination: Point) extends Action
  case class GoToEntity(entityID: EntityActorRef) extends Action
  case class GoHome() extends Action

  sealed trait Transition extends Action
  case class Roam(roamAction: Action) extends Transition
  case class Idle() extends Transition

  sealed trait Destination
  case object Home extends Destination
  case class DestinationPoint(point: Point) extends Destination
  case class DestinationEntity(entityID: EntityActorRef) extends Destination

  private[behaviour] case class Become(behaviour: () => Behavior[EntityBehaviourMessage], data: WalkerData)
      extends ActiveEntity.Become
}
