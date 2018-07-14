package owe.map

import akka.actor.{Actor, ActorRef, Props}
import owe.Tagging.@@
import owe.entities.Entity
import owe.entities.Entity.EntityRef
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Doodad.DoodadRef
import owe.entities.passive.Road.RoadRef
import owe.entities.passive.Roadblock.RoadblockRef
import owe.map.Cell.Availability

class Cell extends Actor {
  import Cell._

  def handler(data: CellData): Receive = {
    case AddEntity(entity) =>
      val updatedData = data.copy(entities = data.entities + (entity.entityRef -> entity))
      context.become(handler(updatedData))

    case RemoveEntity(entityID) =>
      val updatedData = data.copy(entities = data.entities - entityID)
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

    case UpdateBuildAllowed(buildingAllowed) =>
      val updatedData = data.copy(state = data.state.copy(buildingAllowed = buildingAllowed))
      context.become(handler(updatedData))

    case GetEntity(entityID) =>
      sender ! data.entities.get(entityID)

    case GetCellData() =>
      sender ! data

    case GetCellAvailability() =>
      val result: Availability = data.entities
        .find {
          case (_, mapEntity) =>
            mapEntity.entityRef match {
              case _: DoodadRef    => true
              case _: RoadRef      => false
              case _: RoadblockRef => false
              case _: StructureRef => true
              case _: ResourceRef  => true
              case _: WalkerRef    => false
            }
        } match {
        case Some(_) =>
          Availability.Occupied

        case None =>
          if (data.entities.isEmpty) {
            Availability.Buildable
          } else {
            Availability.Passable
          }
      }

      sender ! result

    case HasRoad() =>
      val result = data.entities.exists {
        case (_, entity) =>
          entity.entityRef match {
            case _: RoadRef => true
            case _          => false
          }
      }

      sender ! result
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
  case class UpdateDesirability(desirability: Desirability) extends Message
  case class UpdateFertility(fertility: Fertility) extends Message
  case class UpdateWater(water: Water) extends Message
  case class UpdateBuildAllowed(buildingAllowed: Boolean) extends Message
  case class GetEntity(entityID: EntityRef) extends Message
  case class GetCellData() extends Message
  case class GetCellAvailability() extends Message
  case class HasRoad() extends Message

  trait Effect extends owe.effects.Effect {
    def apply(state: State): State
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

  case class CellData(
    entities: Map[EntityRef, MapEntity],
    state: State
  )

  object CellData {
    def empty: CellData = CellData(
      entities = Map.empty,
      state = State(
        desirability = Desirability.Neutral,
        fertility = Fertility.Min,
        water = Water.Min,
        buildingAllowed = true
      )
    )
  }

  case class State(
    desirability: Desirability,
    fertility: Fertility,
    water: Water,
    buildingAllowed: Boolean
  )

  sealed trait Availability
  object Availability {
    case object Buildable extends Availability
    case object Passable extends Availability
    case object Occupied extends Availability
    case object OutOfBounds extends Availability
  }

  implicit class AvailabilityAsInt(val availability: Availability) extends AnyVal {
    def toInt: Int =
      availability match {
        case Availability.Buildable   => 3
        case Availability.Passable    => 2
        case Availability.Occupied    => 1
        case Availability.OutOfBounds => 0
      }

    def >(other: Availability): Boolean = availability.toInt > other.toInt
    def <(other: Availability): Boolean = availability.toInt < other.toInt
    def >=(other: Availability): Boolean = availability.toInt >= other.toInt
    def <=(other: Availability): Boolean = availability.toInt <= other.toInt
  }

  def requiredAvailability(entityType: Entity.Type): Availability =
    entityType match {
      case Entity.Type.Doodad    => Availability.Buildable
      case Entity.Type.Road      => Availability.Buildable
      case Entity.Type.Roadblock => Availability.Passable //TODO - can be built only on roads w/o walkers
      case Entity.Type.Resource  => Availability.Buildable
      case Entity.Type.Structure => Availability.Buildable
      case Entity.Type.Walker    => Availability.Passable
    }
}
