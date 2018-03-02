package owe.test.specs.unit.entities.definitions.active.resources

import owe.EffectID
import owe.entities.active.Resource

object CopperVein extends Resource {
  override protected def createProperties(): Resource.Properties = Resource.Properties(
    name = "GoldVein",
    maxAmount = 50
  )

  override protected def createState(): Resource.State = Resource.State(
    currentAmount = 100,
    replenishRate = 5,
    replenishAmount = 10
  )

  override protected def createStateModifiers(): Resource.StateModifiers = Resource.StateModifiers(
    replenishAmount = 100,
    replenishRate = 75
  )

  override protected def createEffects(): Map[EffectID, Effect] = Map.empty

  override protected def tick(
    tickSize: Int,
    state: Resource.State,
    modifiers: Resource.StateModifiers
  ): Resource.State = ??? //TODO
}