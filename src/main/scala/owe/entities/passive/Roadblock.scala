package owe.entities.passive

import akka.actor.ActorRef
import owe.EntityDesirability
import owe.Tagging.@@
import owe.entities.passive.Roadblock.ActorRefTag
import owe.entities.{Entity, PassiveEntity}

class Roadblock extends PassiveEntity[ActorRefTag] {
  final override def `type`: Entity.Type = Entity.Type.Roadblock

  final override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Roadblock {
  type PassiveEntityActorRef = ActorRef @@ ActorRefTag
  trait ActorRefTag extends PassiveEntity.ActorRefTag
}
