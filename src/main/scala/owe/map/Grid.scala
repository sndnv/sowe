package owe.map

import breeze.linalg.DenseMatrix
import owe.EntityID
import owe.map.Grid.Failure

trait Grid[T] {
  val gridSize: Int

  //private val grid = DenseMatrix.zeros[T](gridSize, gridSize)

  def addEntity(at: Location, entity: T): Either[(EntityID, Cell.State), Failure]
  def removeEntity(at: Location, id: EntityID): Either[Cell.State, Failure]
  def getEntity(at: Location, id: EntityID): T
  def getCellState(at: Location): Cell.State
  def isCellInGrid(cell: Location): Boolean
  def isCellPassable(cell: Location): Boolean
  def passableNeighbours(cell: Location): Vector[Location]
  def pathBetween(start: Location, end: Location): Option[Vector[Location]]
}

object Grid {
  sealed trait Failure
  object Failure {

    case object GridCellUnavailable extends Failure

    case object EntityMissing extends Failure

    case object EffectPresent extends Failure

    case object EffectMissing extends Failure

  }

  def neighboursOf(cell: Location, withCornerNeighbours: Boolean): Vector[Option[Location]] = {
    val Location(x, y) = cell

    Vector(
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

  def entityCells(entityWidth: Int, entityHeight: Int, parentCell: Location): Vector[Location] = {
    (parentCell.x to entityWidth)
      .flatMap(
        x => (parentCell.y to entityHeight).map(y => Location(x, y))
      )
      .toVector
  }

  def distanceBetween(cell1: Location, cell2: Location): Double = {
    val x = cell2.x - cell1.x
    val y = cell2.y - cell1.y

    Math.sqrt(x * x + y * y)
  }
}
