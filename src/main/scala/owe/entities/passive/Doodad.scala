package owe.entities.passive

import akka.actor.ActorRef
import owe.EntityDesirability
import owe.Tagging.@@
import owe.entities.passive.Doodad.ActorRefTag
import owe.entities.{Entity, PassiveEntity}

class Doodad extends PassiveEntity[ActorRefTag] {
  final override def `type`: Entity.Type = Entity.Type.Doodad

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Doodad {
  type PassiveEntityActorRef = ActorRef @@ ActorRefTag
  trait ActorRefTag extends PassiveEntity.ActorRefTag
}
