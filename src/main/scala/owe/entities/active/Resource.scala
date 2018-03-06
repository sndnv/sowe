package owe.entities.active

import owe.entities._

trait Resource
    extends ActiveEntity[
      Resource.Properties,
      Resource.State,
      Resource.StateModifiers,
      Resource.ActorRefTag
    ] {
  override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
  override def `type`: Entity.Type = Entity.Type.Resource

  override protected def tick(
    tickSize: Int,
    properties: Resource.Properties,
    state: Resource.State,
    modifiers: Resource.StateModifiers
  ): Resource.State = ??? //TODO
}

object Resource {
  trait ActorRefTag extends ActiveEntity.ActorRefTag

  type Effect = ActiveEntity.Effect[Properties, State, StateModifiers]

  case class Properties(
    name: String,
    maxAmount: Int
  ) extends Entity.Properties

  case class State(
    currentAmount: Int,
    replenishRate: Int, //TODO - # of ticks to get `replenishAmount`
    replenishAmount: Int //TODO - amount to replenish after `replenishRate` ticks
  ) extends Entity.State

  case class StateModifiers(
    replenishRate: Int, //TODO - in pct of State.replenishRate
    replenishAmount: Int //TODO - in pct of State.replenishAmount
  ) extends Entity.StateModifiers
}
