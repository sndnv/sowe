package owe.entities.passive

import owe.EntityDesirability
import owe.entities.{Entity, PassiveEntity}

class Roadblock extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Roadblock

  final override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}
