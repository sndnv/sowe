package owe.entities.passive

import owe.entities.Entity
import owe.entities.PassiveEntity

class Road extends PassiveEntity {
  override def `type`: Entity.Type = Entity.Type.Road
}
