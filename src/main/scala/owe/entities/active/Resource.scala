package owe.entities.active

import owe.entities._
import owe.entities.active.behaviour.resource.BaseResource
import owe.map.grid.Point
import owe.production._
import owe.{EntityDesirability, EntityID}

trait Resource
    extends ActiveEntity[
      Resource.Properties,
      Resource.State,
      Resource.StateModifiers,
      BaseResource,
      Resource.ActorRefTag
    ] {
  final override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
  final override def `type`: Entity.Type = Entity.Type.Resource
  final override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Resource {
  type Effect = ActiveEntity.Effect[Properties, State, StateModifiers]

  trait ActorRefTag extends ActiveEntity.ActorRefTag

  case class Properties(
    id: EntityID,
    homePosition: Point,
    name: String,
    commodity: Commodity,
    maxAmount: CommodityAmount
  ) extends Entity.Properties

  case class State(
    currentAmount: CommodityAmount,
    replenishAmount: CommodityAmount //doc - amount to replenish after `replenishRate` ticks
  ) extends Entity.State

  case class StateModifiers(
    replenishAmount: CommodityAmountModifier //doc - in pct of State.replenishAmount
  ) extends Entity.StateModifiers
}
