package owe.entities.passive

import akka.actor.ActorRef
import owe.EntityDesirability
import owe.Tagging.@@
import owe.entities.passive.Road.ActorRefTag
import owe.entities.{Entity, PassiveEntity}

class Road extends PassiveEntity[ActorRefTag] {
  final override def `type`: Entity.Type = Entity.Type.Road

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Road {
  type PassiveEntityActorRef = ActorRef @@ ActorRefTag
  trait ActorRefTag extends PassiveEntity.ActorRefTag
}
