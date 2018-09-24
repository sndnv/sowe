package owe.entities.passive

import akka.actor.ActorRef
import owe.entities.Entity.Desirability
import owe.entities.PassiveEntity.PassiveEntityRef
import owe.entities.{Entity, PassiveEntity}
import owe.map.Cell

class Roadblock extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Roadblock

  final override def `desirability`: Desirability = Desirability.Neutral

  final override def acceptsAvailability(availability: Cell.Availability): Boolean =
    availability.hasRoad && !availability.hasRoadblock && !availability.entityTypes.contains(Entity.Type.Walker)
}

object Roadblock {
  case class RoadblockRef(ref: ActorRef) extends PassiveEntityRef
}
