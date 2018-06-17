package owe.entities.passive

import owe.EntityDesirability
import owe.entities.passive.Roadblock.ActorRefTag
import owe.entities.{Entity, PassiveEntity}

class Roadblock extends PassiveEntity[ActorRefTag] {
  final override def `type`: Entity.Type = Entity.Type.Roadblock

  final override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Roadblock {
  trait ActorRefTag extends PassiveEntity.ActorRefTag
}
