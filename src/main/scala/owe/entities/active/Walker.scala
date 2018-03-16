package owe.entities.active

import owe.EntityDesirability
import owe.entities._
import owe.entities.active.Structure.CommodityAmount
import owe.map.MapCell
import owe.production.Commodity

trait Walker
    extends ActiveEntity[
      Walker.Properties,
      Walker.State,
      Walker.StateModifiers,
      Walker.ActorRefTag
    ] {
  final override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
  final override def `type`: Entity.Type = Entity.Type.Walker
  final override def `desirability`: EntityDesirability = EntityDesirability.Neutral

  override protected def tick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    properties: Walker.Properties,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Walker.State = ??? //TODO

  protected def processMovement(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Seq[owe.Message]

  override protected[entities] def internalAfterTick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Seq[owe.Message] = processMovement(tickSize, cellProperties, cellModifiers, state, modifiers)
}

object Walker {
  trait ActorRefTag extends ActiveEntity.ActorRefTag

  type Effect = ActiveEntity.Effect[Properties, State, StateModifiers]

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
