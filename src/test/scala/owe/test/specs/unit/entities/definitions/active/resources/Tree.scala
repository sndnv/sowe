package owe.test.specs.unit.entities.definitions.active.resources

import owe.effects.Effect
import owe.entities.active.Resource

object Tree extends Resource {
  override protected def createProperties(): Resource.Properties = Resource.Properties(
    name = "Tree",
    maxAmount = 500
  )

  override protected def createState(): Resource.State = Resource.State(
    currentAmount = 0,
    replenishRate = 1,
    replenishAmount = 25
  )

  override protected def createStateModifiers(): Resource.StateModifiers = Resource.StateModifiers(
    replenishRate = 100,
    replenishAmount = 100
  )

  override protected def createEffects(): Seq[((Resource.Properties, Resource.State) => Boolean, Effect)] = Seq.empty
}
