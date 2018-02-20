package owe.map

import java.util.UUID

import breeze.linalg.DenseMatrix
import owe.EntityID
import owe.entities.{EntitySize, EntityType}

trait Grid[E] {
  val height: Int
  val width: Int

  private val grid = DenseMatrix.zeros[Cell[E]](height, width)

  //TODO - refactor
  private def addEntityToCells(cells: Seq[Location], entity: E, entityId: EntityID, entityType: EntityType): Unit = {
    cells.foreach { cell =>
      val gridCell = grid(cell.x, cell.y)
      grid.update(
        cell.x,
        cell.y,
        gridCell.copy(
          entities = gridCell.entities + (entityId -> GridEntity(entity, entityType, cell))
        )
      )
    }
  }

  //TODO - refactor
  private def removeEntityFromCells(cells: Seq[Location], entityId: EntityID): Unit = {
    cells.foreach { cell =>
      val gridCell = grid(cell.x, cell.y)
      grid.update(
        cell.x,
        cell.y,
        gridCell.copy(
          entities = gridCell.entities - entityId
        )
      )
    }
  }

  //TODO - refactor
  def addEntity(
    entity: E,
    entityType: EntityType,
    entitySize: EntitySize,
    cell: Location
  ): Either[Grid.Error, (EntityID, Cell.State)] = {
    val cellState = getCellState(cell)
    cellState match {
      case Cell.State.AvailableEmpty | Cell.State.AvailableOccupied =>
        entityType match {
          case EntityType.Structure =>
            val entityCells = Grid.entityCells(entitySize, cell)
            val cellsAvailable = entityCells.map(getCellState).forall { state =>
              state == Cell.State.AvailableEmpty || state == Cell.State.AvailableOccupied
            }

            if (cellsAvailable) {
              val entityId = UUID.randomUUID()
              addEntityToCells(entityCells, entity, entityId, entityType)
              Right((entityId, cellState))
            } else {
              Left(Grid.Error.CellUnavailable)
            }

          case _ =>
            val entityId = UUID.randomUUID()
            addEntityToCells(Seq(cell), entity, entityId, entityType)
            Right((entityId, cellState))
        }

      case _ =>
        Left(Grid.Error.CellUnavailable)
    }
  }

  //TODO - refactor
  def removeEntity(
    entityId: EntityID,
    entityType: EntityType,
    entitySize: EntitySize,
    cell: Location
  ): Either[Grid.Error, Cell.State] = {
    val cellState = getCellState(cell)
    cellState match {
      case Cell.State.AvailableOccupied | Cell.State.UnavailableOccupied =>
        entityType match {
          case EntityType.Structure =>
            removeEntityFromCells(Grid.entityCells(entitySize, cell), entityId)
            Right(cellState)

          case _ =>
            removeEntityFromCells(Seq(cell), entityId)
            Right(cellState)
        }

      case _ =>
        Left(Grid.Error.CellUnavailable)
    }
  }

  //TODO - refactor
  def getEntity(entityId: EntityID, cell: Location): Either[Grid.Error, E] = {
    val cellState = getCellState(cell)
    cellState match {
      case Cell.State.AvailableEmpty | Cell.State.AvailableOccupied =>
        grid(cell.x, cell.y).entities.get(entityId) match {
          case Some(gridEntity) => Right(gridEntity.entity)
          case None             => Left(Grid.Error.EntityMissing)
        }

      case _ =>
        Left(Grid.Error.CellUnavailable)
    }
  }

  //TODO - refactor
  def getCellState(cell: Location): Cell.State = {
    if (isCellInGrid(cell)) {
      val entities = grid(cell.x, cell.y).entities
      if (entities.isEmpty) {
        Cell.State.AvailableEmpty
      } else if (entities.exists {
                   case (_, entity) =>
                     entity.entityType match {
                       case EntityType.Road      => false
                       case EntityType.Roadblock => false
                       case EntityType.Walker    => false
                       case _                    => true
                     }
                 }) {
        Cell.State.UnavailableOccupied
      } else {
        Cell.State.AvailableOccupied
      }
    } else {
      Cell.State.OutOfBounds
    }
  }

  def isCellInGrid(cell: Location): Boolean = {
    cell.x > 0 && cell.y > 0 && width > cell.x && height > cell.y
  }

  def isCellPassable(cell: Location): Boolean = {
    isCellInGrid(cell) && !grid(cell.x, cell.y).entities.exists {
      case (_, entity) =>
        entity.entityType match {
          case EntityType.Road      => false
          case EntityType.Roadblock => false
          case EntityType.Walker    => false
          case _                    => true
        }
    }
  }

  def passableNeighboursOf(cell: Location): Seq[Location] = {
    //TODO - allow corner neighbours only for specific walkers that don't need roads
    Grid
      .neighboursOf(cell, withCornerNeighbours = true)
      .collect {
        case Some(location) if isCellPassable(location) => location
      }
  }

  def pathBetween(start: Location, end: Location): Option[Seq[Location]] //TODO

  //TODO - effects
}

object Grid {
  sealed trait Error
  object Error {

    case object CellUnavailable extends Error

    case object EntityMissing extends Error

    case object EffectPresent extends Error

    case object EffectMissing extends Error

  }

  def neighboursOf(cell: Location, withCornerNeighbours: Boolean): Seq[Option[Location]] = {
    val Location(x, y) = cell

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

  def entityCells(entitySize: EntitySize, parentCell: Location): Seq[Location] = {
    (parentCell.x to entitySize.width)
      .flatMap(
        x => (parentCell.y to entitySize.height).map(y => Location(x, y))
      )
  }

  def distanceBetween(cell1: Location, cell2: Location): Double = {
    val x = (cell2.x - cell1.x).abs
    val y = (cell2.y - cell1.y).abs

    Math.sqrt(x * x + y * y)
  }
}
