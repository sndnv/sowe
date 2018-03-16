package owe.entities.active

import owe.EntityDesirability
import owe.entities._
import owe.map.MapCell

trait Resource
    extends ActiveEntity[
      Resource.Properties,
      Resource.State,
      Resource.StateModifiers,
      Resource.ActorRefTag
    ] {
  final override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
  final override def `type`: Entity.Type = Entity.Type.Resource
  final override def `desirability`: EntityDesirability = EntityDesirability.Neutral

  override protected def tick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    properties: Resource.Properties,
    state: Resource.State,
    modifiers: Resource.StateModifiers
  ): Resource.State = ??? //TODO
}

object Resource {
  type Effect = ActiveEntity.Effect[Properties, State, StateModifiers]

  trait ActorRefTag extends ActiveEntity.ActorRefTag

  case class Properties(
    name: String,
    maxAmount: Int
  ) extends Entity.Properties

  case class State(
    currentAmount: Int,
    replenishRate: Int, //doc - # of ticks to get `replenishAmount`
    replenishAmount: Int //doc - amount to replenish after `replenishRate` ticks
  ) extends Entity.State

  case class StateModifiers(
    replenishRate: Int, //doc - in pct of State.replenishRate
    replenishAmount: Int //doc - in pct of State.replenishAmount
  ) extends Entity.StateModifiers
}
