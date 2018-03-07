package owe.test.specs.unit.effects.definitions

import owe.entities.active.Walker

class IncreaseMovementSpeed extends Walker.Effect {

  override def radius: Int = 1

  override def apply(
    tickSize: Int,
    properties: Walker.Properties,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Walker.StateModifiers = ??? //TODO
}
