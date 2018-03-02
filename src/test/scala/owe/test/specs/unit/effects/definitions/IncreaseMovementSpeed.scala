package owe.test.specs.unit.effects.definitions

import owe.entities.active.Walker

trait IncreaseMovementSpeed extends Walker.Effect {
  override def apply(
    properties: Walker.Properties,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Walker.StateModifiers = ??? //TODO
}
