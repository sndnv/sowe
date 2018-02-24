package owe.map

import owe.entities.EntityType
import owe.map.grid.Point

case class MapEntity[E](entity: E, entityType: EntityType, parentCell: Point)
