package owe.test.specs.unit.effects.definitions

import owe.entities.ActiveEntity.Data
import owe.entities.active.Structure

class DecreaseFireRisk extends Structure.Effect {

  override def radius: Int = 2

  override def apply(entityData: Data): Structure.StateModifiers = ??? //TODO
}
