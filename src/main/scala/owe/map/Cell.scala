package owe.map

import owe.EntityID

case class Cell(
  entities: Map[EntityID, GridEntity]
)

object Cell {
  sealed trait State
  object State {

    case object AvailableEmpty extends State

    case object AvailableOccupied extends State

    case object UnavailableOccupied extends State

    case object OutOfBounds extends State

  }

  def empty(): Cell = {
    Cell(Map.empty)
  }
}
