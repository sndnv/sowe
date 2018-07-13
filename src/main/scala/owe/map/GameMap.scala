package owe.map

import akka.actor.{Actor, ActorLogging, ActorRef, Stash, Timers}
import akka.pattern.pipe
import akka.util.Timeout
import owe.Tagging._
import owe.entities.ActiveEntity.ActiveEntityRef
import owe.entities.Entity
import owe.entities.Entity._
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.attributes.{AttackDamage, Distance}
import owe.map.Cell.{ActorRefTag, CellActorRef}
import owe.map.grid.{Grid, Point}
import owe.map.ops.Ops
import owe.map.pathfinding.Search
import owe.production.{Commodity, Exchange}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait GameMap extends Actor with ActorLogging with Stash with Timers with Ops {

  import GameMap._

  private case object TickTimer

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext = context.dispatcher

  protected val height: Int
  protected val width: Int
  protected val tickInterval: FiniteDuration
  protected val defaultTickStart: Point = Point(0, 0)
  protected val defaultTickEnd: Point = defaultTickStart

  protected val exchange: ActorRef
  protected val tracker: ActorRef
  protected val search: Search

  private val grid = Grid[CellActorRef](height, width, context.actorOf(Cell.props()).tag[ActorRefTag])
  private var entities: Map[EntityRef, Point] = Map.empty

  private def scheduleNextTick(): Unit =
    if (tickInterval.toMillis > 0L) {
      timers.startSingleTimer(
        TickTimer,
        ProcessTick(defaultTickStart, defaultTickEnd),
        tickInterval
      )
    }

  private def active: Receive = {
    case ProcessTick(start, end) =>
      log.debug("Started processing tick from [{}] to [{}].", start, end)
      processTick(grid, start, end).pipeTo(self)

    case GetAdvancePath(entityID, destination) =>
      log.debug("Generating advance path for entity [{}] to [{}].", entityID, destination)
      sender ! getAdvancePath(grid, entities, entityID, destination)

    case GetRoamingPath(entityID, length) =>
      log.debug("Generating roaming path for entity [{}].", entityID)
      sender ! getRoamingPath(grid, entities, entityID, length)

    case GetNeighbours(entityID, radius) =>
      log.debug("Retrieving neighbours for entity [{}] in radius [{}].", entityID, radius)
      getNeighbours(grid, entities, entityID, radius).pipeTo(sender)

    case GetEntities(point) =>
      log.debug("Retrieving entities in cell [{}]", point)
      getEntities(grid, entities, point).pipeTo(sender)

    case GetEntity(entityID) =>
      log.debug("Retrieving data for entity [{}]", entityID)
      getEntity(grid, entities, entityID).pipeTo(sender)

    case TickProcessed(processedCells) =>
      log.debug("[{}] cells processed by tick.", processedCells)
      scheduleNextTick()
      unstashAll()
      context.become(idle)

    case _ => stash()
  }

  private def idle: Receive = {
    case UpdateEntities(updatedEntities) =>
      entities = updatedEntities

    case CreateEntity(entity, cell) =>
      log.debug("Creating entity of type [{}] with size [{}].", entity.`type`, entity.`size`)

      val mapEntity = MapEntity(
        context.actorOf(entity.props()),
        cell,
        entity.`size`,
        entity.`desirability`,
        entity.`type`
      )

      createEntity(grid, entities, mapEntity, cell)
        .map {
          case (updatedEntities, event) =>
            tracker ! event
            UpdateEntities(updatedEntities)
        }
        .pipeTo(self)

    case DestroyEntity(entityID) =>
      log.debug("Destroying entity with ID [{}.", entityID)

      destroyEntity(grid, entities, entityID)
        .map {
          case (updatedEntities, event) =>
            tracker ! event
            UpdateEntities(updatedEntities)
        }
        .pipeTo(self)

    case MoveEntity(entityID, cell) =>
      log.debug("Moving entity with ID [{}] to [{}].", entityID, cell)

      moveEntity(grid, entities, entityID, cell)
        .map {
          case (updatedEntities, event) =>
            tracker ! event
            UpdateEntities(updatedEntities)
        }
        .pipeTo(self)

    case DistributeCommodities(entityID, commodities) =>
      forwardEntityMessage(grid, entities, entityID, ProcessCommodities(commodities)).pipeTo(sender)

    case AttackEntity(entityID, damage) =>
      forwardEntityMessage(grid, entities, entityID, ProcessAttack(damage)).pipeTo(sender)

    case LabourFound(entityID) =>
      forwardEntityMessage(grid, entities, entityID, ProcessLabourFound()).pipeTo(sender)

    case OccupantsUpdate(entityID, occupants) =>
      forwardEntityMessage(grid, entities, entityID, ProcessOccupantsUpdate(occupants)).pipeTo(sender)

    case LabourUpdate(entityID, employees) =>
      forwardEntityMessage(grid, entities, entityID, ProcessLabourUpdate(employees)).pipeTo(sender)

    case ForwardExchangeMessage(message) =>
      log.debug("Forwarding message [{}] to commodity exchange.", message)
      exchange ! message

    case message: ProcessTick =>
      self ! message
      context.become(active)
  }

  override def receive: Receive = idle

  scheduleNextTick()
}

object GameMap {
  sealed trait Message extends owe.Message

  private case class UpdateEntities(updatedEntities: Map[EntityRef, Point]) extends Message

  private[map] case class ProcessTick(start: Point, end: Point) extends Message
  private[map] case class TickProcessed(processedCells: Int) extends Message

  case class GetAdvancePath(entityID: WalkerRef, destination: Point) extends Message
  case class GetRoamingPath(entityID: WalkerRef, length: Distance) extends Message
  case class GetNeighbours(entityID: EntityRef, radius: Distance) extends Message
  case class GetEntities(point: Point) extends Message
  case class GetEntity(entityID: ActiveEntityRef) extends Message
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
