package owe.entities.passive

import owe.EntityDesirability
import owe.entities.Entity
import owe.entities.PassiveEntity

class Doodad extends PassiveEntity {
  override def `type`: Entity.Type = Entity.Type.Doodad

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}
