package owe.map

import akka.actor.{Actor, ActorLogging, ActorRef, Stash, Timers}
import akka.pattern.pipe
import akka.util.Timeout
import owe.EntityID
import owe.Tagging._
import owe.entities.Entity
import owe.entities.Entity._
import owe.entities.active._
import owe.map.grid.pathfinding.Search
import owe.map.grid.{Grid, Point}
import owe.map.ops.Ops
import owe.production.{Commodity, CommodityAmount, Exchange}

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

  protected val tracker: ActorRef
  protected val search: Search

  protected val exchange: ActorRef

  private val grid = Grid[MapCell](height, width, MapCell.empty)
  private var entities: Map[EntityID, Point] = Map.empty

  private def scheduleNextTick(): Unit =
    timers.startSingleTimer(
      TickTimer,
      ProcessTick(defaultTickStart, defaultTickEnd),
      tickInterval
    )

  private def idle: Receive = {
    case CreateEntity(entity, cell) =>
      log.debug("Creating entity of type [{}] with size [{}].", entity.`type`, entity.`size`)
      entities = createEntity(grid, entities, entity, cell, context.system.actorOf)

    case DestroyEntity(entityID) =>
      log.debug("Destroying entity with ID [{}.", entityID)
      entities = destroyEntity(grid, entities, entityID)

    case MoveEntity(entityID, cell) =>
      log.debug("Moving entity with ID [{}] to [{}].", entityID, cell)
      entities = moveEntity(grid, entities, entityID, cell)

    case DistributeCommodities(entityID, commodities) =>
      forwardEntityMessage(grid, entities, entityID, ProcessCommodities(commodities))

    case AttackEntity(entityID, damage) =>
      forwardEntityMessage(grid, entities, entityID, ProcessAttack(damage))

    case LabourFound(entityID) =>
      forwardEntityMessage(grid, entities, entityID, ProcessLabourFound())

    case OccupantsUpdate(entityID, occupants) =>
      forwardEntityMessage(grid, entities, entityID, ProcessOccupantsUpdate(occupants))

    case LabourUpdate(entityID, employees) =>
      forwardEntityMessage(grid, entities, entityID, ProcessLabourUpdate(employees))

    case ForwardExchangeMessage(message) =>
      log.debug("Forwarding message [{}] to commodity exchange.", message)
      exchange ! message

    case message: ProcessTick =>
      self ! message
      context.become(active)
  }

  private def active: Receive = {
    case ProcessTick(start, end) =>
      log.debug("Started processing tick from [{}] to [{}].", start, end)
      processTick(grid, start, end).pipeTo(self)

    case GetAdvancePath(entityID, destination) =>
      log.debug("Generating advance path for entity [{}] to [{}].", entityID, destination)
      sender() ! getAdvancePath(grid, entities, entityID, destination)

    case GetRoamingPath(entityID, length) =>
      log.debug("Generating roaming path for entity [{}].", entityID)
      sender() ! getRoamingPath(grid, entities, entityID, length)

    case GetNeighbours(entityID, radius) =>
      log.debug("Retrieving neighbours for entity [{}] in radius [{}].", entityID, radius)
      getNeighbours(grid, entities, entityID, radius).pipeTo(sender())

    case GetEntities(point) =>
      log.debug("Retrieving entities in cell [{}]", point)
      getEntities(grid, entities, point).pipeTo(sender())

    case GetEntity(entityID) =>
      log.debug("Retrieving data for entity [{}]", entityID)
      getEntity(grid, entities, entityID).pipeTo(sender())

    case TickProcessed(processedCells) =>
      log.debug("[{}] cells processed by tick.", processedCells)
      scheduleNextTick()
      unstashAll()
      context.become(idle)

    case _ => stash()
  }

  override def receive: Receive = idle

  scheduleNextTick()
}

object GameMap {
  sealed trait Message extends owe.Message
  private[map] case class ProcessTick(start: Point, end: Point) extends Message
  private[map] case class TickProcessed(processedCells: Int) extends Message
  case class GetAdvancePath(entityID: EntityID, destination: Point) extends Message
  case class GetRoamingPath(entityID: EntityID, length: Distance) extends Message
  case class GetNeighbours(entityID: EntityID, radius: Distance) extends Message
  case class GetEntities(point: Point) extends Message
  case class GetEntity(entityID: EntityID) extends Message
  case class CreateEntity(entity: Entity, cell: Point) extends Message
  case class DestroyEntity(entityID: EntityID) extends Message
  case class MoveEntity(entityID: EntityID, cell: Point) extends Message
  case class DistributeCommodities(entityID: EntityID, commodities: Seq[(Commodity, CommodityAmount)]) extends Message
  case class AttackEntity(entityID: EntityID, damage: AttackDamage) extends Message
  case class LabourFound(entityID: EntityID) extends Message
  case class OccupantsUpdate(entityID: EntityID, occupants: Int) extends Message
  case class LabourUpdate(entityID: EntityID, employees: Int) extends Message
  case class ForwardExchangeMessage(message: Exchange.Message) extends Message

  def distanceBetween(cell1: Point, cell2: Point): Double = {
    val x = (cell2.x - cell1.x).abs
    val y = (cell2.y - cell1.y).abs

    Math.sqrt(x * x + y * y)
  }
}
