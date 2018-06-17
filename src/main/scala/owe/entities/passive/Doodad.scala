package owe.entities.passive

import owe.EntityDesirability
import owe.entities.passive.Doodad.ActorRefTag
import owe.entities.{Entity, PassiveEntity}

class Doodad extends PassiveEntity[ActorRefTag] {
  final override def `type`: Entity.Type = Entity.Type.Doodad

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Doodad {
  trait ActorRefTag extends PassiveEntity.ActorRefTag
}
