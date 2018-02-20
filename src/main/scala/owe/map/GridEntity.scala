package owe.map

import owe.entities.EntityType

case class GridEntity[E](entity: E, entityType: EntityType, parentCell: Location)
