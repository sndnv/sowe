package owe.test.specs.unit.effects.definitions

import owe.entities.ActiveEntity.ActiveEntityData
import owe.entities.active.Structure

class DecreaseFireRisk extends Structure.Effect {

  override def radius: Int = 2

  override def apply(entityData: ActiveEntityData): Structure.StateModifiers = ??? //TODO
}
