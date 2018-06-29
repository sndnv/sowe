package owe.map

import owe.EntityDesirability
import owe.entities.Entity
import owe.entities.Entity.EntityActorRef
import owe.entities.active.Resource.ResourceActorRef
import owe.entities.active.Structure.StructureActorRef
import owe.entities.active.Walker.WalkerActorRef
import owe.entities.passive.Doodad.DoodadActorRef
import owe.entities.passive.Road.RoadActorRef
import owe.entities.passive.Roadblock.RoadblockActorRef
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
      case _: DoodadActorRef    => Entity.Type.Doodad
      case _: RoadActorRef      => Entity.Type.Road
      case _: RoadblockActorRef => Entity.Type.Roadblock
      case _: ResourceActorRef  => Entity.Type.Resource
      case _: StructureActorRef => Entity.Type.Structure
      case _: WalkerActorRef    => Entity.Type.Walker
    }
}
