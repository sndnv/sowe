package owe.entities.passive

import owe.EntityDesirability
import owe.entities.Entity.EntityActorRef
import owe.entities.{Entity, PassiveEntity}

class Road extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Road

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Road {
  trait RoadActorRef extends EntityActorRef
}
