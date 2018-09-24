package owe.map

import akka.actor.ActorRef
import owe.entities.Entity
import owe.entities.Entity.EntityRef
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Doodad.DoodadRef
import owe.entities.passive.Road.RoadRef
import owe.entities.passive.Roadblock.RoadblockRef
import owe.map.grid.Point

case class MapEntity(
  entityRef: EntityRef,
  parentCell: Point,
  spec: Entity
) {
  def entityType: Entity.Type =
    entityRef match {
      case _: DoodadRef    => Entity.Type.Doodad
      case _: RoadRef      => Entity.Type.Road
      case _: RoadblockRef => Entity.Type.Roadblock
      case _: ResourceRef  => Entity.Type.Resource
      case _: StructureRef => Entity.Type.Structure
      case _: WalkerRef    => Entity.Type.Walker
    }
}

object MapEntity {
  def apply(
    actorRef: ActorRef,
    parentCell: Point,
    spec: Entity
  ): MapEntity = new MapEntity(
    spec.`type` match {
      case Entity.Type.Doodad    => DoodadRef(actorRef)
      case Entity.Type.Road      => RoadRef(actorRef)
      case Entity.Type.Roadblock => RoadblockRef(actorRef)
      case Entity.Type.Resource  => ResourceRef(actorRef)
      case Entity.Type.Structure => StructureRef(actorRef)
      case Entity.Type.Walker    => WalkerRef(actorRef)
    },
    parentCell,
    spec
  )
}
