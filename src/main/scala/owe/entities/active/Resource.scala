package owe.entities.active

import akka.actor.ActorRef
import owe.entities.ActiveEntity.ActiveEntityRef
import owe.entities.Entity.Desirability
import owe.entities._
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.behaviour.resource.BaseResource
import owe.map.Cell
import owe.map.grid.Point
import owe.production._

trait Resource
    extends ActiveEntity[
      Resource.Properties,
      Resource.State,
      Resource.StateModifiers,
      BaseResource
    ] {
  final override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
  final override def `type`: Entity.Type = Entity.Type.Resource
  final override def `desirability`: Desirability = Desirability.Neutral
  final override protected def actorToActiveEntityRef(ref: ActorRef) = ResourceRef(ref)
  final override def acceptsAvailability(availability: Cell.Availability): Boolean = availability.isFree
}

object Resource {
  type Effect = ActiveEntity.Effect[Properties, State, StateModifiers]

  case class ResourceRef(ref: ActorRef) extends ActiveEntityRef

  case class Properties(
    homePosition: Point,
    name: String,
    commodity: Commodity,
    maxAmount: Commodity.Amount
  ) extends Entity.Properties

  case class State(
    currentAmount: Commodity.Amount,
    replenishAmount: Commodity.Amount //doc - amount to replenish after `replenishRate` ticks
  ) extends Entity.State

  case class StateModifiers(
    replenishAmount: Commodity.AmountModifier //doc - in pct of State.replenishAmount
  ) extends Entity.StateModifiers
}
