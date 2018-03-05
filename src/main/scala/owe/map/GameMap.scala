package owe.map

import java.util.UUID

import akka.actor.Actor
import owe.EntityID
import owe.entities.{ActiveEntity, Entity, PassiveEntity}
import owe.entities.active.{Resource, Structure}
import owe.entities.passive.Doodad
import owe.map.grid.{Grid, Point}
import owe.Tagging._

trait GameMap extends Actor {
  val height: Int
  val width: Int

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
                    entity.`size`
                  )

                case entity: PassiveEntity =>
                  PassiveMapEntity(entity, cell)
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

      case None => Left(GameMap.Error.CellUnavailable)
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

      case None => Left(GameMap.Error.CellUnavailable)
    }

  private def cellAvailability(cell: MapCell): MapCell.Availability =
    cell.entities
      .find {
        case (_, mapEntity) =>
          mapEntity match {
            case PassiveMapEntity(entity, _) =>
              entity match {
                case _: Doodad => true
                case _         => false
              }

            case ActiveMapEntity(entity, _, _) =>
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

  //TODO - effects & expiration
}

object GameMap {

  sealed trait Error
  object Error {
    case object CellUnavailable extends Error
    case object EntityMissing extends Error
    case object EffectPresent extends Error
    case object EffectMissing extends Error
  }

  sealed trait Message extends owe.Message
  case class CreateEntity() extends Message
  case class DestroyEntity() extends Message
  case class AddEffect() extends Message
  case class RemoveEffect() extends Message

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
