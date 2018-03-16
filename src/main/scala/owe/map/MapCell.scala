package owe.map

import owe.entities.Entity
import owe.{CellDesirability, EntityID}

private[map] class MapCell(
  private var cellEntities: Map[EntityID, MapEntity],
  private val cellProperties: MapCell.Properties,
  private var cellModifiers: MapCell.Modifiers
) {
  def entities: Map[EntityID, MapEntity] = cellEntities
  def properties: MapCell.Properties = cellProperties
  def modifiers: MapCell.Modifiers = cellModifiers
  def addEntity(entityID: EntityID, entity: MapEntity): Unit = cellEntities += entityID -> entity
  def removeEntity(entityID: EntityID): Unit = cellEntities -= entityID
  def updateModifiers(updatedModifiers: MapCell.Modifiers): Unit = cellModifiers = updatedModifiers
}

object MapCell {
  def empty[E <: Entity]: MapCell =
    new MapCell(
      Map.empty,
      Properties(desirability = CellDesirability.Neutral, fertility = 0, water = 0, buildingAllowed = true),
      Modifiers(desirability = CellDesirability.Neutral, fertility = 0, water = 0, buildingAllowed = true)
    )

  sealed trait Availability
  trait Effect extends owe.effects.Effect {
    def apply(properties: Properties, modifiers: Modifiers): Modifiers
  }

  case class Properties(
    desirability: CellDesirability,
    fertility: Int,
    water: Int,
    buildingAllowed: Boolean
  ) {
    def toModifiers: Modifiers = Modifiers(
      desirability,
      fertility,
      water,
      buildingAllowed
    )
  }

  case class Modifiers(
    desirability: CellDesirability,
    fertility: Int,
    water: Int,
    buildingAllowed: Boolean
  )

  object Availability {
    case object Buildable extends Availability
    case object Passable extends Availability
    case object Occupied extends Availability
    case object OutOfBounds extends Availability
  }
}
