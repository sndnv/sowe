package owe.map

import akka.actor.ActorRef
import owe.EntityDesirability
import owe.Tagging.@@
import owe.entities.ActiveEntity.ActorRefTag
import owe.entities.{Entity, PassiveEntity}
import owe.map.grid.Point

sealed trait MapEntity {
  def parentCell: Point
  def size: Entity.Size
  def desirability: EntityDesirability
}

final case class PassiveMapEntity(
  entity: PassiveEntity,
  parentCell: Point,
  desirability: EntityDesirability
) extends MapEntity {
  override def size: Entity.Size = entity.`size`
}

final case class ActiveMapEntity(
  entity: ActorRef @@ ActorRefTag,
  parentCell: Point,
  size: Entity.Size,
  desirability: EntityDesirability
) extends MapEntity
