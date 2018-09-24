package owe.entities.passive

import akka.actor.ActorRef
import owe.entities.Entity.Desirability
import owe.entities.PassiveEntity.PassiveEntityRef
import owe.entities.{Entity, PassiveEntity}
import owe.map.Cell

class Road extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Road

  final override def `desirability`: Desirability = Desirability.Neutral

  final override def acceptsAvailability(availability: Cell.Availability): Boolean =
    availability.isPassable && !availability.hasRoad && !availability.hasRoadblock
}

object Road {
  case class RoadRef(ref: ActorRef) extends PassiveEntityRef
}
