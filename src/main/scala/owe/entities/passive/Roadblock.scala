package owe.entities.passive

import akka.actor.ActorRef
import owe.EntityDesirability
import owe.entities.PassiveEntity.PassiveEntityRef
import owe.entities.{Entity, PassiveEntity}

class Roadblock extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Roadblock

  final override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Roadblock {
  case class RoadblockRef(ref: ActorRef) extends PassiveEntityRef
}
