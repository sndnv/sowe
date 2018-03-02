package owe.test.specs.unit.effects.definitions

import owe.entities.active.Structure

trait DecreaseFireRisk extends Structure.Effect {
  override def apply(
    properties: Structure.Properties,
    state: Structure.State,
    modifiers: Structure.StateModifiers
  ): Structure.StateModifiers = ??? //TODO
}
