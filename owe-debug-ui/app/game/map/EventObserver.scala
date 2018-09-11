package game.map

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import game.map.EventObserver.SerializableEvent
import owe.events.Event
import owe.events.Event.{CellEvent, EntityEvent, Identifier, SystemEvent}
import owe.events.Tracker.{AttachEventsObserver, DetachEventsObserver}
import owe.map.MapEntity
import owe.map.grid.Point
import play.api.libs.json._

class EventObserver(tracker: ActorRef, out: ActorRef) extends Actor with ActorLogging {
  private val interestingEvents: Seq[Event.Identifier] = Seq(
    Event.Engine.EntityCreated,
    Event.Engine.EntityDestroyed,
    Event.Engine.EntityMoved,
    Event.Engine.EntityMissing,
    Event.Engine.CellsUnavailable,
    Event.Engine.SpawnPointUnavailable,
    Event.Engine.CellOutOfBounds,
    Event.Engine.DestinationUnreachable,
    Event.Engine.MessageForwarded,
    Event.Engine.UnexpectedEntityFound,
    Event.Engine.UnexpectedEntityResponseReceived,
    Event.Engine.TickExpired,
  )

  override def receive: Receive = {
    case event: SystemEvent => out ! SerializableEvent.fromSystemEvent(event)
    case event: CellEvent   => out ! SerializableEvent.fromCellEvent(event)
    case event: EntityEvent => out ! SerializableEvent.fromEntityEvent(event)

    case message =>
      log.warning(s"[event-observer / $self / $out] Received unexpected message: [$message]")
  }

  override def preStart(): Unit = tracker ! AttachEventsObserver(self, interestingEvents: _*)

  override def postStop(): Unit = tracker ! DetachEventsObserver(self, interestingEvents: _*)
}

object EventObserver {

  case class SerializableEvent(id: Identifier, mapEntity: Option[MapEntity], targetCell: Option[Point])

  object SerializableEvent {
    import game.entities.JsonFormatters._

    def fromSystemEvent(event: SystemEvent): SerializableEvent =
      new SerializableEvent(event.id, mapEntity = None, targetCell = None)

    def fromCellEvent(event: CellEvent): SerializableEvent =
      new SerializableEvent(event.id, mapEntity = None, targetCell = Some(event.targetCell))

    def fromEntityEvent(event: EntityEvent): SerializableEvent =
      new SerializableEvent(event.id, mapEntity = Some(event.mapEntity), targetCell = Some(event.targetCell))

    implicit val eventFormat: Format[SerializableEvent] = Json.format[SerializableEvent]
  }

  def props(tracker: ActorRef, out: ActorRef): Props = Props(
    classOf[EventObserver],
    tracker,
    out
  )
}
