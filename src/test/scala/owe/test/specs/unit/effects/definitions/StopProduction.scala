package owe.test.specs.unit.effects.definitions

import owe.entities.active.Resource

trait StopProduction extends Resource.Effect {
  override def apply(
    properties: Resource.Properties,
    state: Resource.State,
    modifiers: Resource.StateModifiers
  ): Resource.StateModifiers = ??? //TODO
}
