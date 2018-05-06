package owe.test.specs.unit.effects.definitions

import owe.entities.ActiveEntity.ActiveEntityData
import owe.entities.active.Resource

class StopProduction extends Resource.Effect {

  override def radius: Int = 10

  override def apply(entityData: ActiveEntityData): Resource.StateModifiers = ??? //TODO
}
