package owe.entities.active

import akka.actor.ActorRef
import owe.entities.ActiveEntity.{ActiveEntityRef, Data}
import owe.entities.Entity.{Desirability, EntityRef}
import owe.entities._
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.{SpawnLocation, WalkerRef}
import owe.entities.active.attributes._
import owe.entities.active.behaviour.walker.BaseWalker
import owe.map.grid.Point
import owe.production.Commodity

import scala.collection.immutable.Queue

trait Walker
    extends ActiveEntity[
      Walker.Properties,
      Walker.State,
      Walker.StateModifiers,
      BaseWalker
    ] {
  def spawnLocation: SpawnLocation

  final override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
  final override def `type`: Entity.Type = Entity.Type.Walker
  final override def `desirability`: Desirability = Desirability.Neutral
  final override protected def actorToActiveEntityRef(ref: ActorRef) = WalkerRef(ref)
}

object Walker {
  case class WalkerRef(ref: ActorRef) extends ActiveEntityRef

  type Effect = ActiveEntity.Effect[Properties, State, StateModifiers]

  sealed trait PropertiesOnly
  sealed trait StateOnly
  sealed trait StateModifiersOnly

  sealed trait Commodities
  case object NoCommodities extends Commodities with StateOnly
  case class CommoditiesState(available: Map[Commodity, Commodity.Amount], limits: Map[Commodity, Commodity.Amount])
      extends Commodities
      with StateOnly

  sealed trait Attack

  case object NoAttack extends Attack with PropertiesOnly with StateModifiersOnly

  case class AttackProperties(
    rate: AttackRate,
    damage: AttackDamage,
    distance: Distance,
    target: Data => Boolean
  ) extends Attack
      with PropertiesOnly

  case class AttackModifiers(
    rate: AttackRate.Modifier,
    damage: AttackDamage.Modifier,
    distance: Distance.Modifier
  ) extends Attack
      with StateModifiersOnly

  sealed trait MovementMode
  object MovementMode {
    case object Roaming extends MovementMode
    case object Advancing extends MovementMode
    case object Returning extends MovementMode
    case object Idling extends MovementMode
  }

  sealed trait TraversalMode
  object TraversalMode {
    case object RoadRequired extends TraversalMode
    case object RoadPreferred extends TraversalMode
    case object OnLand extends TraversalMode
    case object OnWater extends TraversalMode
  }

  sealed trait SpawnLocation
  object SpawnLocation {
    case class AdjacentRoad(entityID: EntityRef) extends SpawnLocation
    case class AdjacentPoint(entityID: EntityRef) extends SpawnLocation
    case object AtPoint extends SpawnLocation
  }

  case class Properties(
    parent: Option[StructureRef],
    homePosition: Point,
    name: String,
    maxLife: Life,
    movementSpeed: Speed,
    maxRoamingDistance: Distance,
    attack: Attack with PropertiesOnly,
    traversalMode: TraversalMode
  ) extends Entity.Properties

  case class State(
    currentPosition: Point,
    currentLife: Life,
    distanceCovered: Distance,
    commodities: Commodities with StateOnly,
    path: Queue[Point],
    mode: MovementMode
  ) extends Entity.State

  case class StateModifiers(
    movementSpeed: Speed.Modifier,
    maxRoamingDistance: Distance.Modifier,
    attack: Attack with StateModifiersOnly
  ) extends Entity.StateModifiers
}
