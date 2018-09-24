package owe.map

import akka.actor.{Actor, ActorRef, Props}
import owe.Tagging.@@
import owe.entities.Entity
import owe.entities.Entity.EntityRef

class Cell extends Actor {
  import Cell._

  def handler(data: CellData): Receive = {
    case AddEntity(entity) =>
      val updatedData = data.copy(entities = data.entities + (entity.entityRef -> entity))
      context.become(handler(updatedData))

    case RemoveEntity(entityID) =>
      val updatedData = data.copy(entities = data.entities - entityID)
      context.become(handler(updatedData))

    case UpdateType(newType) =>
      val updatedData = data.copy(`type` = newType)
      context.become(handler(updatedData))

    case UpdateDesirability(desirability) =>
      val updatedData = data.copy(state = data.state.copy(desirability = desirability))
      context.become(handler(updatedData))

    case UpdateFertility(fertility) =>
      val updatedData = data.copy(state = data.state.copy(fertility = fertility))
      context.become(handler(updatedData))

    case UpdateWater(water) =>
      val updatedData = data.copy(state = data.state.copy(water = water))
      context.become(handler(updatedData))

    case UpdateRestrictions(restrictions) =>
      val updatedData = data.copy(state = data.state.copy(restrictions = restrictions))
      context.become(handler(updatedData))

    case GetEntity(entityID) =>
      sender ! data.entities.get(entityID)

    case GetCellData() =>
      sender ! data

    case GetCellAvailability() =>
      val availability = Availability(
        data.`type`,
        data.state,
        entityTypes = data.entities.map(_._2.entityType).toSet
      )

      sender ! availability
  }

  override def receive: Receive = handler(CellData.empty)
}

object Cell {
  trait ActorRefTag
  type CellActorRef = ActorRef @@ ActorRefTag

  def props(): Props = Props(classOf[Cell])

  sealed trait Message
  case class AddEntity(entity: MapEntity) extends Message
  case class RemoveEntity(entityID: EntityRef) extends Message
  case class UpdateType(newType: Type) extends Message
  case class UpdateDesirability(desirability: Desirability) extends Message
  case class UpdateFertility(fertility: Fertility) extends Message
  case class UpdateWater(water: Water) extends Message
  case class UpdateRestrictions(restrictions: Restrictions) extends Message
  case class GetEntity(entityID: EntityRef) extends Message
  case class GetCellData() extends Message
  case class GetCellAvailability() extends Message

  trait Effect extends owe.effects.Effect {
    def apply(state: State): State
  }

  case class CellData(
    `type`: Type,
    entities: Map[EntityRef, MapEntity],
    state: State
  )

  object CellData {
    def empty: CellData = CellData(
      `type` = Type.Land,
      entities = Map.empty,
      state = State(
        desirability = Desirability.Neutral,
        fertility = Fertility.Min,
        water = Water.Min,
        restrictions = Restrictions.None
      )
    )
  }

  case class State(
    desirability: Desirability,
    fertility: Fertility,
    water: Water,
    restrictions: Restrictions
  )

  case class Availability(
    cellType: Type,
    cellState: State,
    entityTypes: Set[Entity.Type]
  ) {
    def isFree: Boolean = entityTypes.isEmpty

    def isPassable: Boolean =
      entityTypes.forall {
        case Entity.Type.Structure => false
        case Entity.Type.Doodad    => false
        case _                     => true
      } && cellState.restrictions != Cell.Restrictions.Impassable

    def hasRoad: Boolean = entityTypes.contains(Entity.Type.Road)

    def hasRoadblock: Boolean = entityTypes.contains(Entity.Type.Roadblock)
  }

  final case class Desirability(value: Int) extends AnyVal {
    def +(desirability: Desirability): Desirability = Desirability(value + desirability.value)
    def -(desirability: Desirability): Desirability = Desirability(value - desirability.value)
    def *(multiplier: Int): Desirability = Desirability(value * multiplier)
    def /(divisor: Int): Desirability = Desirability(value / divisor)
    def >(desirability: Desirability): Boolean = value > desirability.value
    def <(desirability: Desirability): Boolean = value < desirability.value
    def >=(desirability: Desirability): Boolean = value >= desirability.value
    def <=(desirability: Desirability): Boolean = value <= desirability.value
    def min(desirability: Desirability): Desirability = Desirability(value.min(desirability.value))
    def max(desirability: Desirability): Desirability = Desirability(value.max(desirability.value))
  }

  object Desirability {
    val Min: Desirability = Desirability(-32)
    val Max: Desirability = Desirability(32)
    val Neutral: Desirability = Desirability(0)
  }

  final case class Fertility(value: Int) extends AnyVal {
    def min(fertility: Fertility): Fertility = Fertility(value.min(fertility.value))
    def max(fertility: Fertility): Fertility = Fertility(value.max(fertility.value))
    def basedOn(water: Water): Fertility = Fertility((water.value * value) / 100)
  }

  object Fertility {
    val Min = Fertility(0)
    val Max = Fertility(100)
  }

  final case class Water(value: Int) extends AnyVal {
    def min(water: Water): Water = Water(value.min(water.value))
    def max(water: Water): Water = Water(value.max(water.value))
  }

  object Water {
    val Max = Water(100)
    val Min = Water(0)
  }

  sealed trait Type
  object Type {
    case object Land extends Type
    case object Water extends Type
    case object Floodplain extends Type
    case object Beach extends Type
  }

  sealed trait Restrictions
  object Restrictions {
    case object None extends Restrictions
    case object Unbuildable extends Restrictions
    case object Impassable extends Restrictions
  }
}
