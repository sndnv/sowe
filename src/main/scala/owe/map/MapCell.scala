package owe.map

import owe.EntityID
import owe.entities.Entity

case class MapCell[E <: Entity](
  entities: Map[EntityID, MapEntity[E]],
  effects: Seq[MapCell.Effect],
  properties: MapCell.Properties,
  modifiers: MapCell.Modifiers
)

object MapCell {
  trait Effect
      extends owe.effects.Effect[
        MapCell.Properties,
        MapCell.State,
        MapCell.Modifiers
      ]

  sealed trait State
  object State {
    case object AvailableEmpty extends State
    case object AvailableOccupied extends State
    case object UnavailableOccupied extends State
    case object OutOfBounds extends State
  }

  case class Properties(
    desirability: Int,
    fertility: Int,
    water: Int,
    buildingAllowed: Boolean
  )

  case class Modifiers(
    desirability: Int,
    fertility: Int,
    water: Int,
    buildingAllowed: Boolean
  )

  def empty[E <: Entity]: MapCell[E] =
    MapCell(
      Map.empty,
      Seq.empty,
      Properties(desirability = 0, fertility = 0, water = 0, buildingAllowed = true),
      Modifiers(desirability = 100, fertility = 100, water = 100, buildingAllowed = true)
    )
}
