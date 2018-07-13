package owe.test.specs.unit.effects.definitions

import owe.entities.ActiveEntity.Data
import owe.entities.active.Walker

class IncreaseMovementSpeed extends Walker.Effect {

  override def radius: Int = 1

  override def apply(entityData: Data): Walker.StateModifiers = ??? //TODO
}
