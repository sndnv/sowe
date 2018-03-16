package owe.map

import java.util.UUID

import akka.actor.{Actor, ActorLogging, Stash, Timers}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import owe.Tagging._
import owe.effects.Effect
import owe.entities.active.{Resource, Structure, Walker}
import owe.entities.passive.{Doodad, Road, Roadblock}
import owe.entities.{ActiveEntity, Entity, PassiveEntity}
import owe.map.MapCell.Availability
import owe.map.grid.{Grid, Point}
import owe.events.Tracker
import owe.map.grid.pathfinding.Search
import owe.{EntityDesirability, EntityID}

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
  protected val defaultTickSize: Int
  protected val defaultTickStart: Point = Point(0, 0)
  protected val defaultTickEnd: Point = defaultTickStart

  protected val tracker: Tracker
  protected val search: Search

  private val grid = Grid[MapCell](height, width, MapCell.empty)
  private var entities: Map[EntityID, Point] = Map.empty

  private def applyDesirability(entityDesirability: EntityDesirability, cells: Seq[Point], modifier: Int): Unit =
    entityDesirability.toMap.foldLeft(Seq.empty[Point]) {
      case (processedCells, (radius, desirability)) =>
        val currentWindowCells =
          cells.flatMap(grid.window(_, radius).toMap).filter {
            case (point, _) => !processedCells.contains(point)
          }

        currentWindowCells.foreach {
          case (_, affectedCell) =>
            affectedCell.updateModifiers(
              affectedCell.modifiers.copy(
                desirability = affectedCell.modifiers.desirability + modifier * desirability
              )
            )
        }

        processedCells ++ currentWindowCells.map(_._1)
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

  private def createEntity(entity: Entity, cell: Point): Either[Error, (EntityID, Availability)] =
    grid.get(cell) match {
      case Some(mapCell) =>
        val targetDesirability = requiredAvailability(entity.`type`)
        cellAvailability(mapCell) match {
          case availability if availability == targetDesirability =>
            val cells = entityCells(entity.`size`, cell)
            if (cells.map(cellAvailability).forall(_ == targetDesirability)) {
              val entityID = UUID.randomUUID()
              val mapEntity: MapEntity = entity match {
                case entity: ActiveEntity[_, _, _, _] =>
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

              Right((entityID, availability))
            } else {
              Left(Error.CellUnavailable)
            }

          case _ => Left(Error.CellUnavailable)
        }

      case None => Left(Error.CellOutOfBounds)
    }

  private def destroyEntity(entityID: EntityID): Either[Error, Availability] =
    entities
      .get(entityID)
      .flatMap { point =>
        grid.get(point).map(mapCell => (point, mapCell))
      } match {
      case Some((point, mapCell)) =>
        cellAvailability(mapCell) match {
          case availability @ (Availability.Occupied | Availability.Passable) =>
            mapCell.entities.get(entityID) match {
              case Some(mapEntity) =>
                dissociateMapEntity(mapEntity, entityID, point)
                Right(availability)

              case None =>
                Left(Error.EntityMissing)
            }

          case _ => Left(Error.EntityMissing)
        }

      case None => Left(Error.CellOutOfBounds)
    }

  private def moveEntity(entityID: EntityID, newCell: Point): Either[Error, Availability] =
    (for {
      currentCell <- entities.get(entityID).toRight(Error.CellUnavailable): Either[Error, Point]
      currentMapCell <- grid.get(currentCell).toRight(Error.EntityMissing): Either[Error, MapCell]
      currentMapEntity <- currentMapCell.entities.get(entityID).toRight(Error.EntityMissing): Either[Error, MapEntity]
      newMapCell <- grid.get(newCell).toRight(Error.CellUnavailable)
    } yield {
      val targetDesirability = requiredAvailability(entityTypeFromMapEntity(currentMapEntity))

      cellAvailability(newMapCell) match {
        case availability if availability == targetDesirability =>
          val cells = entityCells(currentMapEntity.size, newCell)
          if (cells.map(cellAvailability).forall(_ == targetDesirability)) {
            dissociateMapEntity(currentMapEntity, entityID, currentCell)
            associateMapEntity(currentMapEntity.withNewParentCell(newCell), entityID, newCell)

            Right(availability)
          } else {
            Left(Error.CellUnavailable)
          }
      }
    }).joinRight

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
                case _: Doodad => true
                case _         => false
              }

            case ActiveMapEntity(entity, _, _, _) =>
              entity match {
                case _: Structure.ActorRefTag => true
                case _: Resource.ActorRefTag  => true
                case _                        => false
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

  private def pathBetween(start: Point, end: Point): Option[Queue[Point]] =
    search.calculate(start, end, passableNeighboursOf)

  private def cellHasRoad(mapCell: MapCell): Boolean =
    mapCell.entities.exists {
      case (_, entity) =>
        entity match {
          case PassiveMapEntity(passiveEntity, _, _) => passiveEntity.isInstanceOf[Road]
          case _                                     => false
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
    tickSize: Int,
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

            val cellModifiers = cellEffects.foldLeft(cell.properties.toModifiers) {
              case (modifiers, effect) =>
                effect(cell.properties, modifiers)
            }

            cell.entities.foreach {
              case (_, mapEntity) =>
                mapEntity match {
                  case ActiveMapEntity(entity, _, _, _) =>
                    entity ! ActiveEntity.ApplyEffects(tickSize, entityEffects)
                    entity ! ActiveEntity.ProcessTick(tickSize, cell.properties, cellModifiers)

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
      ProcessTick(defaultTickSize, defaultTickStart, defaultTickEnd),
      tickInterval
    )

  private def idle: Receive = {
    case CreateEntity(entity, cell) =>
      log.debug("Creating entity of type [{}] with size [{}]...", entity.`type`, entity.`size`)
      sender ! createEntity(entity, cell)

    case DestroyEntity(entityID) =>
      log.debug("Destroying entity with ID [{}]...", entityID)
      sender ! destroyEntity(entityID)

    case MoveEntity(entityID, cell) =>
      log.debug("Moving entity with ID [{}] to [{}]...", entityID, cell)
      sender ! moveEntity(entityID, cell)

    case message: ProcessTick =>
      self ! message
      context.become(active)
  }

  private def active: Receive = {
    case ProcessTick(tickSize, start, end) =>
      log.debug("Started processing tick with size [{}] from [{}] to [{}].", tickSize, start, end)
      (for {
        activeEffects <- gatherActiveEffects(grid)
        processedCells <- processTick(grid, activeEffects, tickSize, start, end)
      } yield {
        TickProcessed(processedCells)
      }).pipeTo(self)

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
  //TODO - remove and use event tracking
  sealed trait Error
  object Error {
    case object CellUnavailable extends Error
    case object CellOutOfBounds extends Error
    case object EntityMissing extends Error
  }

  sealed trait Message extends owe.Message
  private case class ProcessTick(tickSize: Int, start: Point, end: Point) extends Message
  private case class TickProcessed(processedCells: Int) extends Message
  case class CreateEntity(entity: Entity, cell: Point) extends Message
  case class DestroyEntity(entityID: EntityID) extends Message
  case class MoveEntity(entityID: EntityID, cell: Point) extends Message

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
