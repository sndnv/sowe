package owe.map

import owe._
import owe.entities.Entity

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
  def empty[E <: Entity]: MapCell = {
    val defaultProperties = Properties(
      desirability = CellDesirability.Neutral,
      fertility = Fertility.Min,
      water = Water.Min,
      buildingAllowed = true
    )
    val defaultModifiers = Modifiers(
      desirability = CellDesirabilityModifier(100),
      fertility = FertilityModifier(100),
      water = WaterModifier(100),
      buildingAllowed = true
    )

    new MapCell(
      Map.empty,
      defaultProperties,
      defaultModifiers
    )
  }

  sealed trait Availability
  trait Effect extends owe.effects.Effect {
    def apply(properties: Properties, modifiers: Modifiers): Modifiers
  }

  case class Properties(
    desirability: CellDesirability,
    fertility: Fertility, //doc - in pct
    water: Water, //doc - in pct
    buildingAllowed: Boolean
  )

  case class Modifiers(
    desirability: CellDesirabilityModifier,
    fertility: FertilityModifier, //doc - in pct of properties.fertility
    water: WaterModifier, //doc - in pct of properties.water
    buildingAllowed: Boolean
  )

  object Availability {
    case object Buildable extends Availability
    case object Passable extends Availability
    case object Occupied extends Availability
    case object OutOfBounds extends Availability
  }
}
