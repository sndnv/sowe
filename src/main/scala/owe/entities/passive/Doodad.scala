package owe.entities.passive

import akka.actor.typed.ActorRef
import owe.EntityDesirability
import owe.entities.Entity.EntityActorRef
import owe.entities.{Entity, PassiveEntity}

class Doodad extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Doodad

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Doodad {
  trait DoodadActorRef extends EntityActorRef
}
