package owe.entities.passive

import owe.EntityDesirability
import owe.entities.passive.Road.ActorRefTag
import owe.entities.{Entity, PassiveEntity}

class Road extends PassiveEntity[ActorRefTag] {
  final override def `type`: Entity.Type = Entity.Type.Road

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Road {
  trait ActorRefTag extends PassiveEntity.ActorRefTag
}
