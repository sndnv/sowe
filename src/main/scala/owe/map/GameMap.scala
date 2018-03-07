package owe.map

import java.util.UUID

import akka.pattern.ask
import akka.actor.{Actor, ActorLogging}
import akka.util.Timeout
import owe.EntityID
import owe.entities.{ActiveEntity, Entity, PassiveEntity}
import owe.entities.active.{Resource, Structure}
import owe.entities.passive.{Doodad, Road}
import owe.map.grid.{Grid, Point}
import owe.Tagging._
import owe.effects.Effect

import scala.concurrent.{ExecutionContext, Future}

class GameMap(
  val height: Int,
  val width: Int
)(implicit ec: ExecutionContext, timeout: Timeout)
    extends Actor
    with ActorLogging {

  private val grid = Grid[MapCell](height, width, MapCell.empty)
  private var entities: Map[EntityID, Point] = Map.empty

  private def createEntity(entity: Entity, cell: Point): Either[GameMap.Error, (EntityID, MapCell.Availability)] =
    grid.get(cell) match {
      case Some(mapCell) =>
        cellAvailability(mapCell) match {
          case availability @ (MapCell.Availability.AvailableEmpty | MapCell.Availability.AvailableOccupied) =>
            val cells = GameMap.entityCells(entity.`size`, cell)
            if (cells.forall(isCellPassable)) {
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

              entities += entityID -> cell

              cells
                .flatMap(grid.get)
                .foreach(_.addEntity(entityID, mapEntity))

              Right((entityID, availability))
            } else {
              Left(GameMap.Error.CellUnavailable)
            }

          case _ => Left(GameMap.Error.CellUnavailable)
        }

      case None => Left(GameMap.Error.CellOutOfBounds)
    }

  private def destroyEntity(entityID: EntityID): Either[GameMap.Error, MapCell.Availability] =
    entities.get(entityID).flatMap(grid.get) match {
      case Some(mapCell) =>
        cellAvailability(mapCell) match {
          case availability @ (MapCell.Availability.AvailableOccupied | MapCell.Availability.UnavailableOccupied) =>
            mapCell.entities.get(entityID) match {
              case Some(mapEntity) =>
                GameMap
                  .entityCells(mapEntity.size, mapEntity.parentCell)
                  .flatMap(grid.get)
                  .foreach(_.removeEntity(entityID))

                entities -= entityID

                Right(availability)

              case None => Left(GameMap.Error.EntityMissing)
            }

          case _ => Left(GameMap.Error.EntityMissing)
        }

      case None => Left(GameMap.Error.CellOutOfBounds)
    }

  private def cellAvailability(cell: MapCell): MapCell.Availability =
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
        MapCell.Availability.UnavailableOccupied

      case None =>
        if (cell.entities.isEmpty) {
          MapCell.Availability.AvailableEmpty
        } else {
          MapCell.Availability.AvailableOccupied
        }
    }

  private def cellAvailability(cell: Point): MapCell.Availability =
    grid
      .get(cell)
      .map(cellAvailability)
      .getOrElse(MapCell.Availability.OutOfBounds)

  private def isCellPassable(cell: Point): Boolean =
    cellAvailability(cell) match {
      case MapCell.Availability.AvailableEmpty      => true
      case MapCell.Availability.AvailableOccupied   => true
      case MapCell.Availability.UnavailableOccupied => false
      case MapCell.Availability.OutOfBounds         => false
    }

  private def passableNeighboursOf(cell: Point): Seq[Point] =
    //TODO - allow corner neighbours only for specific walkers that don't need roads
    GameMap
      .neighboursOf(cell, withCornerNeighbours = true)
      .collect {
        case Some(point) if isCellPassable(point) => point
      }

  private def pathBetween(start: Point, end: Point): Option[Seq[Point]] = ??? //TODO

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
      val entityCells = GameMap.entityCells(mapEntity.size, parentCell)
      entityCells
        .flatMap(point => grid.indexes().window(point, radius = 1).toSeq)
        .distinct
        .flatMap(point => grid.get(point).map(cell => (point, cell)))
        .collect {
          case (point, cell) if !entityCells.contains(point) && cellHasRoad(cell) => point
        }
        .sorted
        .headOption
    }
  }.flatten

  private def tick(tickSize: Int): Future[Int] = {
    val start = Point(0, 0)

    def processCell(currentCell: Point, processedCells: Int): Future[Int] =
      grid
        .get(currentCell)
        .map { cell =>
          Future
            .sequence(
              cell.entities.map {
                case (_, mapEntity) =>
                  mapEntity match {
                    case PassiveMapEntity(_, _, desirability) =>
                      Future.successful(desirability, Seq.empty)

                    case ActiveMapEntity(entity, _, _, desirability) =>
                      (entity ? ActiveEntity.GetActiveEffects()).mapTo[Seq[Effect]].map { effects =>
                        (desirability, effects)
                      }
                  }
              }
            )
            .flatMap { data =>
              val (desirability, effects) = data.unzip

              //TODO - apply desirability

              effects.flatten.foreach { effect =>
                grid.window(currentCell, effect.radius).foreach { cell =>
                  effect match {
                    case cellEffect: MapCell.Effect => //TODO - handle cell effects

                    case entityEffect: ActiveEntity.Effect[_, _, _] =>
                      cell.entities.foreach {
                        case (_, mapEntity) =>
                          mapEntity match {
                            case ActiveMapEntity(entity, _, _, _) =>
                              entity ! ActiveEntity.ApplyEffects(tickSize, Seq(entityEffect))

                            case _ => () //do nothing
                          }
                      }
                  }
                }
              }

              cell.entities.foreach {
                case (_, mapEntity) =>
                  mapEntity match {
                    case ActiveMapEntity(entity, _, _, _) =>
                      entity ! ActiveEntity.ProcessTick(tickSize)

                    case _ => () //do nothing
                  }
              }

              grid
                .nextPoint(currentCell)
                .map { nextCell =>
                  if (nextCell == start) {
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

  override def receive: Receive = {
    case GameMap.CreateEntity()  => //TODO
    case GameMap.DestroyEntity() => //TODO
  }
}

object GameMap {

  sealed trait Error
  object Error {
    case object CellUnavailable extends Error
    case object CellOutOfBounds extends Error
    case object EntityMissing extends Error
  }

  sealed trait Message extends owe.Message
  case class CreateEntity() extends Message
  case class DestroyEntity() extends Message

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
