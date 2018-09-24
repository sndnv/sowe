package owe.map

import java.util.UUID

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Stash, Timers}
import akka.pattern.pipe
import akka.util.Timeout
import owe.Tagging._
import owe.entities.ActiveEntity.ActiveEntityRef
import owe.entities.ActiveEntityActor.AddEntityMessage
import owe.entities.Entity
import owe.entities.Entity._
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.attributes.{AttackDamage, Distance}
import owe.events.Event
import owe.events.Event.SystemEvent
import owe.map.Cell.{Availability, CellActorRef}
import owe.map.grid.{Grid, Point}
import owe.map.ops.Ops
import owe.map.pathfinding.Search
import owe.production.{Commodity, Exchange}

trait GameMap extends Actor with ActorLogging with Stash with Timers with Ops {

  import GameMap._

  private case object TickTimer
  private case object CollectionTimer

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext = context.dispatcher

  protected val height: Int
  protected val width: Int
  protected val collectionTimeout: FiniteDuration
  protected val tickInterval: FiniteDuration
  protected val defaultTickStart: Point = Point(0, 0)
  protected val defaultTickEnd: Point = Point(width - 1, height - 1)

  protected val exchange: ActorRef
  protected val tracker: ActorRef
  protected val search: Search

  private val grid = Grid[CellActorRef](height, width, context.actorOf(Cell.props()).tag[Cell.ActorRefTag])

  protected def waiting(entities: Map[EntityRef, Point], currentTick: Int, pendingEntityResponses: Int): Receive = {
    case GetAdvancePath(entityID, destination) =>
      log.debug("Generating advance path for entity [{}] to [{}]", entityID, destination)
      getAdvancePath(grid, entities, entityID, destination).pipeTo(sender)

    case GetRoamingPath(entityID, length) =>
      log.debug("Generating roaming path for entity [{}]", entityID)
      getRoamingPath(grid, entities, entityID, length).pipeTo(sender)

    case GetNeighbours(entityID, radius) =>
      log.debug("Retrieving neighbours for entity [{}] in radius [{}]", entityID, radius)
      getNeighbours(grid, entities, entityID, radius).pipeTo(sender)

    case GetEntities(point) =>
      log.debug("Retrieving entities in cell [{}]", point)
      getEntities(grid, point).pipeTo(sender)

    case GetEntity(entityID) =>
      log.debug("Retrieving data for entity [{}]", entityID)
      getEntity(grid, entities, entityID).pipeTo(sender)

    case GetAdjacentPoint(entityID, matchesAvailability) =>
      log.debug("Retrieving adjacent point for entity [{}]", entityID)
      findFirstAdjacentPoint(grid, entities, entityID, matchesAvailability).pipeTo(sender)

    case TickExpired(tick) =>
      log.error(
        "Tick [{}] expired while waiting for tick [{}] completion with [{}] pending entity responses",
        tick,
        currentTick,
        pendingEntityResponses
      )
      scheduleNextTick()
      unstashAll()
      tracker ! SystemEvent(Event.Engine.TickExpired)
      context.become(idle(entities, currentTick + 1))

    case EntityTickProcessed(tick) =>
      if (tick == currentTick) {
        if (pendingEntityResponses > 1) {
          log.debug(
            "Entity response for tick [{}] received; [{}] responses remaining",
            currentTick,
            pendingEntityResponses - 1
          )
          context.become(waiting(entities, currentTick, pendingEntityResponses - 1))
        } else {
          log.debug("Processing of tick [{}] complete", currentTick)
          timers.cancel(CollectionTimer)
          scheduleNextTick()
          unstashAll()
          tracker ! SystemEvent(Event.Engine.TickProcessed)
          context.become(idle(entities, currentTick + 1))
        }
      } else {
        log.error(
          "Entity response received from sender [{}] for tick [{}] while waiting on tick [{}]",
          sender,
          tick,
          currentTick
        )
        tracker ! SystemEvent(Event.Engine.UnexpectedEntityResponseReceived)
      }

    case message =>
      log.debug("Stashing message [{}] from sender [{}] while waiting", message, sender)
      stash()
  }

  protected def active(entities: Map[EntityRef, Point], currentTick: Int): Receive = {
    case ProcessTick(start, end) =>
      log.debug("Started processing tick [{}] from [{}] to [{}]", currentTick, start, end)
      timers.startSingleTimer(CollectionTimer, TickExpired(currentTick), collectionTimeout)
      processTick(grid, currentTick, start, end).pipeTo(self)

    case TickProcessed(processedEntities) =>
      log.debug("Entities processed by tick: [{}]", processedEntities)
      if (processedEntities == 0) { self ! EntityTickProcessed(currentTick) }
      unstashAll()
      context.become(waiting(entities, currentTick, pendingEntityResponses = processedEntities))

    case message =>
      log.debug("Stashing message [{}] from sender [{}] while active", message, sender)
      stash()
  }

  protected def idle(entities: Map[EntityRef, Point], currentTick: Int): Receive = {
    case update: EntityUpdate =>
      log.debug("Updating entities: [{}]", update)

      val updatedEntities = update match {
        case EntityUpdate.Add(mapEntity, cell) =>
          associateMapEntity(grid, entities, mapEntity, cell)

        case EntityUpdate.Remove(mapEntity, cell) =>
          dissociateMapEntity(grid, entities, mapEntity, cell)

        case EntityUpdate.Move(mapEntity, from, to) =>
          associateMapEntity(
            grid,
            dissociateMapEntity(
              grid,
              entities,
              mapEntity,
              from
            ),
            mapEntity.copy(parentCell = to),
            to
          )
      }

      context.become(idle(updatedEntities, currentTick))

    case CreateEntity(entity, cell) =>
      log.debug("Creating entity of type [{}] with size [{}]", entity.`type`, entity.`size`)

      val senderRef = sender

      val actorRef = context.actorOf(entity.props(), name = s"${entity.`type`}-${UUID.randomUUID()}")

      createEntity(grid, entities, entity, actorRef, cell)
        .foreach {
          case Left(event) =>
            tracker ! event
            actorRef ! PoisonPill

          case Right((mapEntity, event)) =>
            tracker ! event
            if (senderRef != context.system.deadLetters) { senderRef ! mapEntity.entityRef }
            self ! EntityUpdate.Add(mapEntity, mapEntity.parentCell)
        }

    case DestroyEntity(entityID) =>
      log.debug("Destroying entity with ID [{}", entityID)

      destroyEntity(grid, entities, entityID)
        .foreach {
          case Left(event) =>
            tracker ! event

          case Right((event, mapEntity, cell)) =>
            tracker ! event
            self ! EntityUpdate.Remove(mapEntity, cell)
        }

    case MoveEntity(entityID, cell) =>
      log.debug("Moving entity with ID [{}] to [{}]", entityID, cell)

      moveEntity(grid, entities, entityID, cell)
        .foreach {
          case Left(event) =>
            tracker ! event

          case Right((event, mapEntity, from)) =>
            tracker ! event
            entityID ! AddEntityMessage(ProcessMovement(updatedPosition = cell))
            self ! EntityUpdate.Move(mapEntity, from, to = cell)
        }

    case message @ DistributeCommodities(entityID, commodities) =>
      log.debug("Forwarding entity message [{}]", message)
      forwardEntityMessage(grid, entities, entityID, ProcessCommodities(commodities))
        .foreach(event => tracker ! event)

    case message @ AttackEntity(entityID, damage) =>
      log.debug("Forwarding entity message [{}]", message)
      forwardEntityMessage(grid, entities, entityID, ProcessAttack(damage))
        .foreach(event => tracker ! event)

    case message @ LabourFound(entityID) =>
      log.debug("Forwarding entity message [{}]", message)
      forwardEntityMessage(grid, entities, entityID, ProcessLabourFound())
        .foreach(event => tracker ! event)

    case message @ OccupantsUpdate(entityID, occupants) =>
      log.debug("Forwarding entity message [{}]", message)
      forwardEntityMessage(grid, entities, entityID, ProcessOccupantsUpdate(occupants))
        .foreach(event => tracker ! event)

    case message @ LabourUpdate(entityID, employees) =>
      log.debug("Forwarding entity message [{}]", message)
      forwardEntityMessage(grid, entities, entityID, ProcessLabourUpdate(employees))
        .foreach(event => tracker ! event)

    case ForwardExchangeMessage(message) =>
      log.debug("Forwarding message [{}] to commodity exchange", message)
      exchange ! message

    case message: ProcessTick =>
      log.debug("Changing state to active to begin tick processing")
      self ! message
      context.become(active(entities, currentTick))

    case EntityTickProcessed(tick) =>
      log.error("Entity response received from sender [{}] for tick [{}] while idle", sender, tick)
      tracker ! SystemEvent(Event.Engine.UnexpectedEntityResponseReceived)

    case GetGrid() =>
      log.debug("Retrieving grid data")
      getGridData(grid).pipeTo(sender)

    case GetEntities(point) =>
      log.debug("Retrieving entities in cell [{}]", point)
      getEntities(grid, point).pipeTo(sender)

    case GetEntity(entityID) =>
      log.debug("Retrieving data for entity [{}]", entityID)
      getEntity(grid, entities, entityID).pipeTo(sender)
  }

  override def receive: Receive = idle(entities = Map.empty, currentTick = 0)

  private def scheduleNextTick(): Unit =
    if (tickInterval.toMillis > 0L) {
      log.debug("Next game tick scheduled to run in [{} ms]", tickInterval.toMillis)
      timers.startSingleTimer(
        TickTimer,
        ProcessTick(defaultTickStart, defaultTickEnd),
        tickInterval
      )
    } else {
      log.debug("Game ticks disabled by interval set to [{} ms]", tickInterval.toMillis)
    }

  scheduleNextTick()
}

object GameMap {

  sealed trait Message extends owe.Message

  sealed trait EntityUpdate extends Message
  object EntityUpdate {
    case class Add(mapEntity: MapEntity, cell: Point) extends EntityUpdate
    case class Remove(mapEntity: MapEntity, cell: Point) extends EntityUpdate
    case class Move(mapEntity: MapEntity, from: Point, to: Point) extends EntityUpdate
  }

  private[map] case class ProcessTick(start: Point, end: Point) extends Message
  private[map] case class TickProcessed(processedEntities: Int) extends Message
  private[map] case class TickExpired(tick: Int) extends Message
  case class EntityTickProcessed(tick: Int) extends Message

  case class GetGrid() extends Message
  case class GetAdvancePath(entityID: WalkerRef, destination: Point) extends Message
  case class GetRoamingPath(entityID: WalkerRef, length: Distance) extends Message
  case class GetNeighbours(entityID: EntityRef, radius: Distance) extends Message
  case class GetEntities(point: Point) extends Message
  case class GetEntity(entityID: ActiveEntityRef) extends Message
  case class GetAdjacentPoint(entityID: ActiveEntityRef, matchesAvailability: Availability => Boolean) extends Message
  case class CreateEntity(entity: Entity, cell: Point) extends Message
  case class DestroyEntity(entityID: EntityRef) extends Message
  case class MoveEntity(entityID: EntityRef, cell: Point) extends Message
  case class DistributeCommodities(entityID: ActiveEntityRef, commodities: Seq[(Commodity, Commodity.Amount)])
      extends Message
  case class AttackEntity(entityID: EntityRef, damage: AttackDamage) extends Message
  case class LabourFound(entityID: StructureRef) extends Message
  case class OccupantsUpdate(entityID: StructureRef, occupants: Int) extends Message
  case class LabourUpdate(entityID: StructureRef, employees: Int) extends Message
  case class ForwardExchangeMessage(message: Exchange.Message) extends Message
}
