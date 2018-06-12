package owe.map

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Stash, Timers}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import owe.Tagging._
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, AddEntityMessage, GetData, MapData}
import owe.entities.Entity._
import owe.entities.active._
import owe.entities.passive.{Doodad, Road, Roadblock}
import owe.entities.{ActiveEntity, Entity, PassiveEntity}
import owe.events.Event
import owe.map.MapCell.Availability
import owe.map.grid.pathfinding.Search
import owe.map.grid.{Grid, Point}
import owe.production.{Commodity, CommodityAmount, Exchange}
import owe.{CellDesirabilityModifier, EntityDesirability, EntityID}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.concurrent.duration._

trait GameMap extends Actor with ActorLogging with Stash with Timers {
  import GameMap._
  import context.dispatcher

  private case object TickTimer

  protected implicit val actionTimeout: Timeout

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

  private def applyDesirability(entityDesirability: EntityDesirability, cells: Seq[Point], modifier: Int): Unit = {
    val _ = entityDesirability.toMap.foldLeft(Seq.empty[Point]) {
      case (processedCells, (radius, desirability)) =>
        val currentWindowCells =
          cells.flatMap(grid.window(_, radius).toMap).filter {
            case (point, _) => !processedCells.contains(point)
          }

        currentWindowCells.foreach {
          case (_, affectedCell) =>
            val updatedModifier = affectedCell.modifiers.desirability.value + modifier * desirability.value
            affectedCell.updateModifiers(
              affectedCell.modifiers.copy(
                desirability = CellDesirabilityModifier(updatedModifier)
              )
            )
        }

        processedCells ++ currentWindowCells.map(_._1)
    }
  }

  private def addDesirability(entityDesirability: EntityDesirability, cells: Seq[Point]): Unit =
    applyDesirability(entityDesirability, cells, modifier = 1)

  private def removeDesirability(entityDesirability: EntityDesirability, cells: Seq[Point]): Unit =
    applyDesirability(entityDesirability, cells, modifier = -1)

  private def associateMapEntity(mapEntity: MapEntity, entityID: EntityID, cell: Point): Unit = {
    val cells = entityCells(mapEntity.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_.addEntity(entityID, mapEntity))

    entities += entityID -> cell

    mapEntity match {
      case PassiveMapEntity(entity, _, desirability) =>
        entity match {
          case _: Doodad | _: Road => addDesirability(desirability, cells)
          case _                   => () //do nothing
        }

      case ActiveMapEntity(entity, _, _, desirability) =>
        entity match {
          case _: Structure.ActorRefTag => addDesirability(desirability, cells)
          case _                        => () //do nothing
        }
    }
  }

  private def dissociateMapEntity(mapEntity: MapEntity, entityID: EntityID, cell: Point): Unit = {
    val cells = entityCells(mapEntity.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_.removeEntity(entityID))

    entities -= entityID

    mapEntity match {
      case PassiveMapEntity(entity, _, desirability) =>
        entity match {
          case _: Doodad | _: Road => removeDesirability(desirability, cells)
          case _                   => () //do nothing
        }

      case ActiveMapEntity(entity, _, _, desirability) =>
        entity match {
          case _: Structure.ActorRefTag => removeDesirability(desirability, cells)
          case _                        => () //do nothing
        }
    }
  }

  private def createEntity(entity: Entity, cell: Point): Unit =
    grid.get(cell) match {
      case Some(mapCell) =>
        val targetAvailability = requiredAvailability(entity.`type`)
        cellAvailability(mapCell) match {
          case availability if availability == targetAvailability =>
            val cells = entityCells(entity.`size`, cell)
            if (cells.map(cellAvailability).forall(_ == targetAvailability)) {
              val entityID = UUID.randomUUID()
              val mapEntity: MapEntity = entity match {
                case entity: ActiveEntity[_, _, _, _, _] =>
                  ActiveMapEntity(
                    context.system.actorOf(entity.props()).tag[entity.Tag],
                    cell,
                    entity.`size`,
                    entity.`desirability`
                  )

                case entity: PassiveEntity =>
                  PassiveMapEntity(entity, cell, entity.`desirability`)
              }

              associateMapEntity(mapEntity, entityID, cell)

              tracker ! Event(Event.System.EntityCreated, Some(cell))
            } else {
              tracker ! Event(Event.System.CellsUnavailable, Some(cell))
            }

          case _ =>
            tracker ! Event(Event.System.CellsUnavailable, Some(cell))
        }

      case None =>
        tracker ! Event(Event.System.CellOutOfBounds, Some(cell))
    }

  private def destroyEntity(entityID: EntityID): Unit =
    entities
      .get(entityID)
      .flatMap { point =>
        grid.get(point).map(mapCell => (point, mapCell))
      } match {
      case Some((cell, mapCell)) =>
        cellAvailability(mapCell) match {
          case Availability.Occupied | Availability.Passable =>
            mapCell.entities.get(entityID) match {
              case Some(mapEntity) =>
                dissociateMapEntity(mapEntity, entityID, cell)
                tracker ! Event(Event.System.EntityDestroyed, Some(cell))

              case None =>
                tracker ! Event(Event.System.EntityMissing, Some(cell))
            }

          case _ =>
            tracker ! Event(Event.System.EntityMissing, Some(cell))
        }

      case None =>
        tracker ! Event(Event.System.CellOutOfBounds, cell = None)
    }

  private def moveEntity(entityID: EntityID, newCell: Point): Unit = {
    val event = (for {
      currentCell <- entities
        .get(entityID)
        .toRight(Event(Event.System.CellsUnavailable, cell = None)): Either[Event, Point]
      currentMapCell <- grid
        .get(currentCell)
        .toRight(Event(Event.System.EntityMissing, Some(currentCell))): Either[Event, MapCell]
      currentMapEntity <- currentMapCell.entities
        .get(entityID)
        .toRight(Event(Event.System.EntityMissing, Some(currentCell))): Either[Event, MapEntity]
      newMapCell <- grid
        .get(newCell)
        .toRight(Event(Event.System.CellsUnavailable, Some(newCell))): Either[Event, MapCell]
    } yield {
      val targetAvailability = requiredAvailability(entityTypeFromMapEntity(currentMapEntity))

      cellAvailability(newMapCell) match {
        case availability if availability == targetAvailability =>
          val cells = entityCells(currentMapEntity.size, newCell)
          if (cells.map(cellAvailability).forall(_ == targetAvailability)) {
            dissociateMapEntity(currentMapEntity, entityID, currentCell)
            associateMapEntity(currentMapEntity.withNewParentCell(newCell), entityID, newCell)

            Event(Event.System.EntityMoved, Some(newCell))
          } else {
            Event(Event.System.CellsUnavailable, Some(newCell))
          }
      }
    }).merge

    tracker ! event
  }

  private def entityTypeFromMapEntity(mapEntity: MapEntity): Entity.Type =
    mapEntity match {
      case PassiveMapEntity(entity, _, _) =>
        entity match {
          case _: Doodad    => Entity.Type.Doodad
          case _: Road      => Entity.Type.Road
          case _: Roadblock => Entity.Type.Roadblock
        }

      case ActiveMapEntity(entity, _, _, _) =>
        entity match {
          case _: Resource.ActorRefTag  => Entity.Type.Resource
          case _: Structure.ActorRefTag => Entity.Type.Structure
          case _: Walker.ActorRefTag    => Entity.Type.Walker
        }
    }

  private def cellAvailability(cell: MapCell): Availability =
    cell.entities
      .find {
        case (_, mapEntity) =>
          mapEntity match {
            case PassiveMapEntity(entity, _, _) =>
              entity match {
                case _: Doodad    => true
                case _: Road      => false
                case _: Roadblock => false
              }

            case ActiveMapEntity(entity, _, _, _) =>
              entity match {
                case _: Structure.ActorRefTag => true
                case _: Resource.ActorRefTag  => true
                case _: Walker.ActorRefTag    => false
              }
          }
      } match {
      case Some(_) =>
        Availability.Occupied

      case None =>
        if (cell.entities.isEmpty) {
          Availability.Buildable
        } else {
          Availability.Passable
        }
    }

  private def cellAvailability(cell: Point): Availability =
    grid
      .get(cell)
      .map(cellAvailability)
      .getOrElse(Availability.OutOfBounds)

  private def requiredAvailability(entityType: Entity.Type): Availability = entityType match {
    case Entity.Type.Doodad    => Availability.Buildable
    case Entity.Type.Road      => Availability.Buildable
    case Entity.Type.Roadblock => Availability.Passable //TODO - can be built only on roads w/o walkers
    case Entity.Type.Resource  => Availability.Buildable
    case Entity.Type.Structure => Availability.Buildable
    case Entity.Type.Walker    => Availability.Passable
  }

  private def passableNeighboursOf(cell: Point): Seq[Point] =
    //TODO - allow corner neighbours only for specific walkers that don't need roads
    //TODO - handle roadblocks for specific walkers
    GameMap
      .neighboursOf(cell, withCornerNeighbours = true)
      .collect {
        case Some(point) if cellAvailability(point) == Availability.Passable => point
      }

  //TODO - check if path should follow roads
  private def generateAdvancePath(start: Point, end: Point): Option[Queue[Point]] =
    search.calculate(start, end, passableNeighboursOf)

  //TODO - check if path should follow roads
  private def generateRoamPath(start: Point, maxDistance: Distance): Option[Queue[Point]] = {
    @tailrec
    def extendPath(
      currentCell: Point,
      currentPath: Seq[Point],
      examined: Seq[Point],
      backtracked: Seq[Point]
    ): Seq[Point] = {
      val maxDistanceNotReached = currentPath.lengthCompare(maxDistance.value) < 0
      if (maxDistanceNotReached) {
        passableNeighboursOf(currentCell).filterNot { neighbour =>
          examined.contains(neighbour)
        }.headOption match {
          case Some(nextCell) =>
            extendPath(nextCell, currentPath :+ currentCell, examined :+ currentCell, backtracked)

          case None =>
            val backtrackedTooFar = backtracked.lengthCompare(maxDistance.value / 2) >= 0
            if (backtrackedTooFar) {
              currentPath ++ backtracked
            } else {
              currentPath.lastOption match {
                case Some(previousCell) =>
                  extendPath(previousCell,
                             currentPath.dropRight(1),
                             examined :+ currentCell,
                             backtracked :+ currentCell)

                case None =>
                  Seq.empty
              }
            }
        }
      } else {
        currentPath
      }
    }

    extendPath(start, currentPath = Seq.empty, examined = Seq.empty, backtracked = Seq.empty) match {
      case Nil  => None
      case path => Some(path.to[Queue])
    }
  }

  private def cellHasRoad(mapCell: MapCell): Boolean =
    mapCell.entities.exists {
      case (_, entity) =>
        entity match {
          case PassiveMapEntity(passiveEntity, _, _) =>
            passiveEntity match {
              case _: Road => true
              case _       => false
            }

          case _ => false
        }
    }

  private def findFirstAdjacentRoad(entityID: EntityID): Option[Point] = {
    for {
      parentCell <- entities.get(entityID)
      mapCell <- grid.get(parentCell)
      mapEntity <- mapCell.entities.get(entityID)
    } yield {
      val cells = entityCells(mapEntity.size, parentCell)
      cells
        .flatMap(point => grid.indexes().window(point, radius = 1).toSeq)
        .distinct
        .flatMap(point => grid.get(point).map(cell => (point, cell)))
        .collect {
          case (point, cell) if !cells.contains(point) && cellHasRoad(cell) => point
        }
        .sorted
        .headOption
    }
  }.flatten

  //doc -> a map of points -> effects to be applied to those points
  private def gatherActiveEffects(grid: Grid[MapCell]): Future[Map[Point, Seq[Effect]]] = {
    val indexedGrid = grid.indexes()
    Future
      .traverse(
        grid.toMap.mapValues { mapCell =>
          Future
            .sequence(
              mapCell.entities.map {
                case (_, mapEntity) =>
                  mapEntity match {
                    case ActiveMapEntity(entity, _, _, _) =>
                      (entity ? ActiveEntity.GetActiveEffects()).mapTo[Seq[Effect]]

                    case _: PassiveMapEntity =>
                      Future.successful(Seq.empty)
                  }
              }
            )
            .map(_.flatten)
        }
      ) {
        case (point, future) =>
          future.map { result =>
            result.map(effect => (effect, indexedGrid.window(point, effect.radius).toSeq))
          }
      }
      .map { result =>
        result.flatten.foldLeft(Map.empty[Point, Seq[Effect]]) {
          case (map, (effect, points)) =>
            val updates = points.map { point =>
              (point, map.getOrElse(point, Seq.empty) :+ effect)
            }

            map ++ updates
        }
      }
  }

  //doc -> end is inclusive
  private def processTick(
    grid: Grid[MapCell],
    activeEffects: Map[Point, Seq[Effect]],
    start: Point,
    end: Point
  ): Future[Int] = {
    def processCell(currentCell: Point, processedCells: Int): Future[Int] =
      grid
        .get(currentCell)
        .flatMap { cell =>
          activeEffects.get(currentCell).map { effects =>
            val (cellEffects, entityEffects) =
              effects.foldLeft((Seq.empty[MapCell.Effect], Seq.empty[Effect])) {
                case ((ce, ee), effect) =>
                  effect match {
                    case effect: MapCell.Effect => (ce :+ effect, ee)
                    case _                      => (ce, ee :+ effect)
                  }
              }

            val cellModifiers = cellEffects.foldLeft(cell.modifiers) {
              case (modifiers, effect) =>
                effect(cell.properties, modifiers)
            }

            cell.entities.foreach {
              case (_, mapEntity) =>
                mapEntity match {
                  case ActiveMapEntity(entity, _, _, _) =>
                    entity ! ActiveEntity.ApplyEffects(entityEffects)
                    entity ! ActiveEntity.ProcessGameTick(MapData(mapEntity.parentCell, cell.properties, cellModifiers))

                  case _ => () //do nothing
                }
            }

            grid
              .nextPoint(currentCell)
              .map { nextCell =>
                if (nextCell == end) {
                  Future.successful(processedCells + 1)
                } else {
                  processCell(nextCell, processedCells + 1)
                }
              }
              .getOrElse(Future.failed(new IllegalStateException(s"Failed to find next cell after [$currentCell]")))
          }
        }
        .getOrElse(Future.failed(new IllegalArgumentException(s"Failed to find cell at [$currentCell]")))

    processCell(currentCell = start, processedCells = 0)
  }

  private def scheduleNextTick(): Unit =
    timers.startSingleTimer(
      TickTimer,
      ProcessTick(defaultTickStart, defaultTickEnd),
      tickInterval
    )

  private def idle: Receive = {
    case CreateEntity(entity, cell) =>
      log.debug("Creating entity of type [{}] with size [{}].", entity.`type`, entity.`size`)
      createEntity(entity, cell)

    case DestroyEntity(entityID) =>
      log.debug("Destroying entity with ID [{}.", entityID)
      destroyEntity(entityID)

    case MoveEntity(entityID, cell) =>
      log.debug("Moving entity with ID [{}] to [{}].", entityID, cell)
      moveEntity(entityID, cell)

    case DistributeCommodities(entityID, commodities) =>
      forwardEntityMessage(entityID, ProcessCommodities(commodities))

    case AttackEntity(entityID, damage) =>
      forwardEntityMessage(entityID, ProcessAttack(damage))

    case LabourFound(entityID) =>
      forwardEntityMessage(entityID, ProcessLabourFound())

    case OccupantsUpdate(entityID, occupants) =>
      forwardEntityMessage(entityID, ProcessOccupantsUpdate(occupants))

    case LabourUpdate(entityID, employees) =>
      forwardEntityMessage(entityID, ProcessLabourUpdate(employees))

    case ForwardExchangeMessage(message) =>
      log.debug("Forwarding message [{}] to commodity exchange.", message)
      exchange ! message

    case message: ProcessTick =>
      self ! message
      context.become(active)
  }

  private def forwardEntityMessage(entityID: EntityID, message: Entity.Message): Unit = {
    log.debug("Forwarding message [{}] to entity with ID [{}]", message, entityID)

    (for {
      parentCell <- entities.get(entityID)
      mapCell <- grid.get(parentCell)
      mapEntity <- mapCell.entities.get(entityID)
    } yield {
      mapEntity match {
        case activeEntity: ActiveMapEntity =>
          activeEntity.entity ! AddEntityMessage(message)

        case _: PassiveMapEntity =>
          log.error("Can't forward message [{}] to passive entity with ID [{}].", message, entityID)
      }
    }) match {
      case Some(_) => log.debug("Message [{}] forwarded to entity with ID [{}].", message, entityID)
      case None    => log.error("Failed to find entity with ID [{}] while processing message [{}].", entityID, message)
    }
  }

  private def active: Receive = {
    case ProcessTick(start, end) =>
      log.debug("Started processing tick from [{}] to [{}].", start, end)
      (for {
        activeEffects <- gatherActiveEffects(grid)
        processedCells <- processTick(grid, activeEffects, start, end)
      } yield {
        TickProcessed(processedCells)
      }).pipeTo(self)

    case GetAdvancePath(entityID, destination) =>
      log.debug("Generating advance path for entity [{}] to [{}].", entityID, destination)
      sender() ! entities.get(entityID).flatMap(start => generateAdvancePath(start, destination))

    case GetRoamingPath(entityID, length) =>
      log.debug("Generating roaming path for entity [{}].", entityID)
      sender() ! entities.get(entityID).flatMap(start => generateRoamPath(start, length))

    case GetNeighbours(entityID, radius) =>
      log.debug("Retrieving neighbours for entity [{}] in radius [{}].", entityID, radius)
      val result: Future[Seq[(EntityID, ActiveEntityData)]] = Future
        .traverse(
          entities
            .get(entityID)
            .map { point =>
              grid
                .window(point, radius.value)
                .toSeq
                .flatMap { mapCell =>
                  mapCell.entities.collect {
                    case (k, v: ActiveMapEntity) => (k, (v.entity ? GetData()).mapTo[ActiveEntityData])
                  }
                }
                .toMap
            }
            .getOrElse(Map.empty)
        ) {
          case (neighbourEntityID, future) =>
            future.map { result =>
              (neighbourEntityID, result)
            }
        }
        .map(_.toSeq)

      result.pipeTo(sender())

    case GetEntities(point) =>
      log.debug("Retrieving entities in cell [{}]", point)
      val result: Future[Seq[(MapEntity, Option[ActiveEntityData])]] = Future
        .sequence(
          grid
            .get(point)
            .map { mapCell =>
              mapCell.entities.values.toSeq.map {
                case entity: ActiveMapEntity =>
                  (entity.entity ? GetData()).mapTo[ActiveEntityData].map(data => (entity, Some(data)))

                case entity: PassiveMapEntity =>
                  Future.successful(entity, None)
              }
            }
            .getOrElse(Seq.empty)
        )

      result.pipeTo(sender())

    case GetEntity(entityID) =>
      log.debug("Retrieving data for entity [{}]", entityID)
      (entities
        .get(entityID)
        .flatMap(grid.get)
        .flatMap(_.entities.get(entityID))
        .collect {
          case ActiveMapEntity(entity, _, _, _) =>
            entity ? GetData()
        } match {
        case Some(result) =>
          result

        case None =>
          val message = s"Failed to find entity with ID [$entityID] while retrieving its data."
          log.error(message)
          Future.failed(new IllegalStateException(message))
      }).pipeTo(sender())

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
  private case class ProcessTick(start: Point, end: Point) extends Message
  private case class TickProcessed(processedCells: Int) extends Message
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

  def neighboursOf(cell: Point, withCornerNeighbours: Boolean): Seq[Option[Point]] = {
    val Point(x, y) = cell

    Seq(
      /* top    left   */ if (withCornerNeighbours && x > 0 && y > 0) Some((x - 1, y - 1)) else None,
      /* top    center */ if (y > 0) Some((x, y - 1)) else None,
      /* top    right  */ if (withCornerNeighbours && y > 0) Some((x + 1, y - 1)) else None,
      /* middle left   */ if (x > 0) Some((x - 1, y)) else None,
      /* middle right  */ Some((x + 1, y)),
      /* bottom left   */ if (withCornerNeighbours && x > 0) Some((x - 1, y + 1)) else None,
      /* bottom center */ Some((x, y + 1)),
      /* bottom right  */ if (withCornerNeighbours) Some((x + 1, y + 1)) else None
    )
  }

  def entityCells(entitySize: Entity.Size, parentCell: Point): Seq[Point] =
    (parentCell.x to entitySize.width)
      .flatMap(
        x => (parentCell.y to entitySize.height).map(y => Point(x, y))
      )

  def distanceBetween(cell1: Point, cell2: Point): Double = {
    val x = (cell2.x - cell1.x).abs
    val y = (cell2.y - cell1.y).abs

    Math.sqrt(x * x + y * y)
  }
}
