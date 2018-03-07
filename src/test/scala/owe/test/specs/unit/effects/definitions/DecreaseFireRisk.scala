package owe.test.specs.unit.effects.definitions

import owe.entities.active.Structure

class DecreaseFireRisk extends Structure.Effect {

  override def radius: Int = 2

  override def apply(
    tickSize: Int,
    properties: Structure.Properties,
    state: Structure.State,
    modifiers: Structure.StateModifiers
  ): Structure.StateModifiers = ??? //TODO
}
