package owe.entities.passive

import akka.actor.ActorRef
import owe.entities.Entity.Desirability
import owe.entities.PassiveEntity.PassiveEntityRef
import owe.entities.{Entity, PassiveEntity}

class Roadblock extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Roadblock

  final override def `desirability`: Desirability = Desirability.Neutral
}

object Roadblock {
  case class RoadblockRef(ref: ActorRef) extends PassiveEntityRef
}
