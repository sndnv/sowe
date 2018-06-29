package owe.map

import akka.actor.{Actor, ActorRef, Props}
import owe.Tagging.@@
import owe._
import owe.entities.Entity
import owe.entities.Entity.EntityActorRef
import owe.entities.active.Resource.ResourceActorRef
import owe.entities.active.Structure.StructureActorRef
import owe.entities.active.Walker.WalkerActorRef
import owe.entities.passive.Doodad.DoodadActorRef
import owe.entities.passive.Road.RoadActorRef
import owe.entities.passive.Roadblock.RoadblockActorRef

class Cell extends Actor {
  import Cell._

  def handler(data: CellData): Receive = {
    case AddEntity(entityID, entity) =>
      val updatedData = data.copy(entities = data.entities + (entityID -> entity))
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
              case _: DoodadActorRef    => true
              case _: RoadActorRef      => false
              case _: RoadblockActorRef => false
              case _: StructureActorRef => true
              case _: ResourceActorRef  => true
              case _: WalkerActorRef    => false
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
            case _: RoadActorRef => true
            case _               => false
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
  case class AddEntity(entityID: EntityActorRef, entity: MapEntity) extends Message
  case class RemoveEntity(entityID: EntityActorRef) extends Message
  case class UpdateDesirability(desirability: CellDesirability) extends Message
  case class UpdateFertility(fertility: Fertility) extends Message
  case class UpdateWater(water: Water) extends Message
  case class UpdateBuildAllowed(buildingAllowed: Boolean) extends Message
  case class GetEntity(entityID: EntityActorRef) extends Message
  case class GetCellData() extends Message
  case class GetCellAvailability() extends Message
  case class HasRoad() extends Message

  trait Effect extends owe.effects.Effect {
    def apply(state: State): State
  }

  case class CellData(
    entities: Map[EntityActorRef, MapEntity],
    state: State
  )

  object CellData {
    def empty: CellData = CellData(
      entities = Map.empty,
      state = State(
        desirability = CellDesirability.Neutral,
        fertility = Fertility.Min,
        water = Water.Min,
        buildingAllowed = true
      )
    )
  }

  case class State(
    desirability: CellDesirability,
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
