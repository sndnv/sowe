package owe.test.specs.unit.effects.definitions

import owe.entities.ActiveEntity.Data
import owe.entities.active.Resource

class StopProduction extends Resource.Effect {

  override def radius: Int = 10

  override def apply(entityData: Data): Resource.StateModifiers = ??? //TODO
}
