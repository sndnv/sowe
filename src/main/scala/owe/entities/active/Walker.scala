package owe.entities.active

import owe.entities.active.Structure.CommodityAmount
import owe.entities._
import owe.production.Commodity

trait Walker
    extends ActiveEntity[
      Walker.Properties,
      Walker.State,
      Walker.StateModifiers
    ] {
  override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
  override def `type`: Entity.Type = Entity.Type.Walker

  protected def tick(
    tickSize: Int,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Walker.State

  protected def processMovement(
    tickSize: Int,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Seq[owe.Message]

  override protected def afterTick(
    tickSize: Int,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Seq[owe.Message] = processMovement(tickSize, state, modifiers)
}

object Walker {
  type Effect = Entity.Effect[Properties, State, StateModifiers]

  case class Properties(
    name: String,
    interactionDistance: Int,
    patrolDistance: Option[Int],
    movementSpeed: Int,
    maxLife: Int,
    attackRate: Option[Int],
    attackDamage: Option[Int],
  ) extends Entity.Properties

  case class State(
    currentLife: Int,
    availableCommodities: Map[Commodity, CommodityAmount]
  ) extends Entity.State

  case class StateModifiers(
    interactionDistance: Int,
    patrolDistance: Option[Int],
    movementSpeed: Int,
    attackRate: Option[Int],
    attackDamage: Option[Int],
  ) extends Entity.StateModifiers
}
