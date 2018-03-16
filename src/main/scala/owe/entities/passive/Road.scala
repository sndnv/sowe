package owe.entities.passive

import owe.EntityDesirability
import owe.entities.Entity
import owe.entities.PassiveEntity

class Road extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Road

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}
