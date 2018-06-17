package owe.map

import owe.EntityDesirability
import owe.entities.ActiveEntity.ActiveEntityActorRef
import owe.entities.Entity
import owe.entities.Entity.EntityActorRef
import owe.entities.PassiveEntity.PassiveEntityActorRef
import owe.map.grid.Point

sealed trait MapEntity {
  def entity: EntityActorRef
  def parentCell: Point
  def size: Entity.Size
  def desirability: EntityDesirability
  def withNewParentCell(newParentCell: Point): MapEntity
}

final case class PassiveMapEntity(
  entity: PassiveEntityActorRef,
  parentCell: Point,
  size: Entity.Size,
  desirability: EntityDesirability
) extends MapEntity {
  override def withNewParentCell(newParentCell: Point): MapEntity = copy(parentCell = newParentCell)
}

final case class ActiveMapEntity(
  entity: ActiveEntityActorRef,
  parentCell: Point,
  size: Entity.Size,
  desirability: EntityDesirability
) extends MapEntity {
  override def withNewParentCell(newParentCell: Point): MapEntity = copy(parentCell = newParentCell)
}
