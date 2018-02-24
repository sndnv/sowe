package owe.map

import owe.EntityID
import owe.map.Cell.{Modifiers, Properties}

case class Cell[E](
  entities: Map[EntityID, MapEntity[E]],
  properties: Properties,
  modifiers: Modifiers
)

object Cell {
  sealed trait State
  object State {

    case object AvailableEmpty extends State

    case object AvailableOccupied extends State

    case object UnavailableOccupied extends State

    case object OutOfBounds extends State

  }

  case class Properties(desirability: Int, fertility: Int, water: Int, buildingAllowed: Boolean)

  case class Modifiers(desirability: Int, fertility: Int, water: Int)

  def empty[E](): Cell[E] = {
    Cell(
      Map.empty,
      Properties(desirability = 0, fertility = 0, water = 0, buildingAllowed = true),
      Modifiers(desirability = 100, fertility = 100, water = 100)
    )
  }
}
