package owe.entities.active

import akka.actor.ActorRef
import owe.EntityDesirability
import owe.Tagging.@@
import owe.entities.ActiveEntity.ActiveEntityData
import owe.entities._
import owe.entities.active.behaviour.walker.BaseWalker
import owe.map.grid.Point
import owe.production.{Commodity, CommodityAmount}

import scala.collection.immutable.Queue

trait Walker
    extends ActiveEntity[
      Walker.Properties,
      Walker.State,
      Walker.StateModifiers,
      BaseWalker,
      Walker.ActorRefTag
    ] {
  final override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
  final override def `type`: Entity.Type = Entity.Type.Walker
  final override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Walker {
  type ActiveEntityActorRef = ActorRef @@ ActorRefTag
  trait ActorRefTag extends ActiveEntity.ActorRefTag

  type Effect = ActiveEntity.Effect[Properties, State, StateModifiers]

  sealed trait PropertiesOnly
  sealed trait StateOnly
  sealed trait StateModifiersOnly

  sealed trait Commodities
  case object NoCommodities extends Commodities with StateOnly
  case class CommoditiesState(available: Map[Commodity, CommodityAmount], limits: Map[Commodity, CommodityAmount])
      extends Commodities
      with StateOnly

  sealed trait Attack

  case object NoAttack extends Attack with PropertiesOnly with StateModifiersOnly

  case class AttackProperties(
    rate: AttackRate,
    damage: AttackDamage,
    distance: Distance,
    target: ActiveEntityData => Boolean
  ) extends Attack
      with PropertiesOnly

  case class AttackModifiers(
    rate: AttackRateModifier,
    damage: AttackDamageModifier,
    distance: DistanceModifier
  ) extends Attack
      with StateModifiersOnly

  sealed trait MovementMode
  object MovementMode {
    case object Roaming extends MovementMode
    case object Advancing extends MovementMode
    case object Returning extends MovementMode
    case object Idling extends MovementMode
  }

  case class Properties(
    parent: Option[Structure.ActiveEntityActorRef],
    homePosition: Point,
    name: String,
    maxLife: Life,
    movementSpeed: Speed,
    maxRoamingDistance: Distance,
    attack: Attack with PropertiesOnly
  ) extends Entity.Properties

  case class State(
    currentLife: Life,
    distanceCovered: Distance,
    destinationPath: Queue[Point],
    commodities: Commodities with StateOnly,
    path: Queue[Point],
    mode: MovementMode
  ) extends Entity.State

  case class StateModifiers(
    movementSpeed: SpeedModifier,
    maxRoamingDistance: DistanceModifier,
    attack: Attack with StateModifiersOnly
  ) extends Entity.StateModifiers
}
