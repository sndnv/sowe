package owe.map

import akka.actor.Actor
import owe.EntityID
import owe.entities.Entity
import owe.map.grid.{Grid, Point}

trait GameMap[E <: Entity] extends Actor {
  val height: Int
  val width: Int

  private val grid = Grid[MapCell[E]](height, width, MapCell.empty[E])

  private def addEntity(entity: E, cell: Point): Either[GameMap.Error, (EntityID, MapCell.State)] = ??? //TODO

  private def removeEntity(entityId: EntityID, cell: Point): Either[GameMap.Error, MapCell.State] = ??? //TODO

  private def entity(entityId: EntityID, cell: Point): Either[GameMap.Error, E] = ??? //TODO

  private def cellState(cell: Point): MapCell.State = ??? //TODO

  private def isCellInGrid(cell: Point): Boolean =
    cell.x > 0 && cell.y > 0 && width > cell.x && height > cell.y

  private def isCellPassable(cell: Point): Boolean = ??? //TODO

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
  case class AddEntity() extends Message
  case class RemoveEntity() extends Message
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
