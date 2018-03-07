package owe.entities.passive

import owe.EntityDesirability
import owe.entities.Entity
import owe.entities.PassiveEntity

class Roadblock extends PassiveEntity {
  override def `type`: Entity.Type = Entity.Type.Roadblock

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}
