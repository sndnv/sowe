package owe.events

import owe.events.Event.Identifier
import owe.map.grid.Point

case class Event(id: Identifier, cell: Option[Point])

object Event {
  sealed trait Identifier

  sealed trait System extends Identifier
  object System {
    case object EntityCreated extends System
    case object EntityDestroyed extends System
    case object EntityMoved extends System
    case object EntityMissing extends System
    case object CellsUnavailable extends System
    case object SpawnPointUnavailable extends System
    case object CellOutOfBounds extends System
    case object DestinationUnreachable extends System
    case object MessageForwarded extends System
    case object UnexpectedEntityFound extends System
    case object UnexpectedEntityResponseReceived extends System
    case object TickProcessed extends System
    case object TickExpired extends System
  }

  //doc - to be extended per game
  trait Game extends Identifier
}
