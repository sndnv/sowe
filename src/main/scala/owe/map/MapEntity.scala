package owe.map

import owe.EntityDesirability
import owe.entities.Entity
import owe.entities.Entity.EntityActorRef
import owe.entities.active.{Resource, Structure, Walker}
import owe.entities.passive.{Doodad, Road, Roadblock}
import owe.map.grid.Point

case class MapEntity(
  entityRef: EntityActorRef,
  parentCell: Point,
  size: Entity.Size,
  desirability: EntityDesirability
) {
  def withNewParentCell(newParentCell: Point): MapEntity = copy(parentCell = newParentCell)

  def entityType: Entity.Type =
    entityRef match {
      case _: Doodad                => Entity.Type.Doodad
      case _: Road                  => Entity.Type.Road
      case _: Roadblock             => Entity.Type.Roadblock
      case _: Resource.ActorRefTag  => Entity.Type.Resource
      case _: Structure.ActorRefTag => Entity.Type.Structure
      case _: Walker.ActorRefTag    => Entity.Type.Walker
    }
}
