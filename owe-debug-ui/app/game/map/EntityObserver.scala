package game.map

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import owe.events.Event
import owe.events.Event.EntityEvent
import owe.events.Tracker.{AttachEventsObserver, DetachEventsObserver}

class EntityObserver(tracker: ActorRef, out: ActorRef) extends Actor with ActorLogging {
  private val interestingEvents: Seq[Event.Identifier] = Seq(
    Event.Engine.EntityCreated,
    Event.Engine.EntityDestroyed,
    Event.Engine.EntityMoved
  )

  override def receive: Receive = {
    case event: EntityEvent =>
      out ! event

    case message =>
      log.warning(s"[entity-observer / $self / $out] Received unexpected message: [$message]")
  }

  override def preStart(): Unit = tracker ! AttachEventsObserver(self, interestingEvents: _*)

  override def postStop(): Unit = tracker ! DetachEventsObserver(self, interestingEvents: _*)
}

object EntityObserver {
  def props(tracker: ActorRef, out: ActorRef): Props = Props(
    classOf[EntityObserver],
    tracker,
    out
  )
}
