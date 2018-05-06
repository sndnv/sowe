package owe.test.specs.unit.effects.definitions

import owe.entities.ActiveEntity.ActiveEntityData
import owe.entities.active.Walker

class IncreaseMovementSpeed extends Walker.Effect {

  override def radius: Int = 1

  override def apply(entityData: ActiveEntityData): Walker.StateModifiers = ??? //TODO
}
