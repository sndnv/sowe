package owe.test.specs.unit.effects.definitions

import owe.entities.active.Resource

class StopProduction extends Resource.Effect {

  override def radius: Int = 10

  override def apply(
    tickSize: Int,
    properties: Resource.Properties,
    state: Resource.State,
    modifiers: Resource.StateModifiers
  ): Resource.StateModifiers = ??? //TODO
}
