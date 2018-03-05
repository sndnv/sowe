package owe.map

import owe.EntityID
import owe.entities.Entity

class MapCell(
  private var cellEntities: Map[EntityID, MapEntity],
  private val properties: MapCell.Properties
) {
  def entities: Map[EntityID, MapEntity] = cellEntities
  def addEntity(entityID: EntityID, entity: MapEntity): Unit = cellEntities += entityID -> entity
  def removeEntity(entityID: EntityID): Unit = cellEntities -= entityID
}

object MapCell {
  trait Effect extends owe.effects.Effect {
    def apply(properties: Properties): Properties
  }

  sealed trait Availability
  object Availability {
    case object AvailableEmpty extends Availability
    case object AvailableOccupied extends Availability
    case object UnavailableOccupied extends Availability
    case object OutOfBounds extends Availability
  }

  case class Properties(
    desirability: Int,
    fertility: Int,
    water: Int,
    buildingAllowed: Boolean
  )

  def empty[E <: Entity]: MapCell =
    new MapCell(
      Map.empty,
      Properties(desirability = 0, fertility = 0, water = 0, buildingAllowed = true)
    )
}
