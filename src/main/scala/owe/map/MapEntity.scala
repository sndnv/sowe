package owe.map

import owe.entities.Entity
import owe.map.grid.Point

case class MapEntity[E <: Entity](entity: E, parentCell: Point)
