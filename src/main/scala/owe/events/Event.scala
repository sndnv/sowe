package owe.events

import owe.map.MapEntity
import owe.map.grid.Point

trait Event {
  def id: Event.Identifier
}

object Event {
  sealed trait Identifier

  sealed trait Engine extends Identifier
  object Engine {
    case object EntityCreated extends Engine
    case object EntityDestroyed extends Engine
    case object EntityMoved extends Engine
    case object EntityMissing extends Engine
    case object CellsUnavailable extends Engine
    case object SpawnPointUnavailable extends Engine
    case object CellOutOfBounds extends Engine
    case object DestinationUnreachable extends Engine
    case object MessageForwarded extends Engine
    case object UnexpectedEntityFound extends Engine
    case object UnexpectedEntityResponseReceived extends Engine
    case object TickProcessed extends Engine
    case object TickExpired extends Engine
  }

  //doc - to be extended per game
  trait Game extends Identifier

  case class SystemEvent(id: Identifier) extends Event
  case class CellEvent(id: Identifier, targetCell: Point) extends Event
  case class EntityEvent(id: Identifier, mapEntity: MapEntity, targetCell: Point) extends Event
}
