package owe.events

import akka.actor.{Actor, ActorRef, Props}
import owe.events.Tracker._

class Tracker() extends Actor {
  private def track(observers: Map[Event.Identifier, Seq[ActorRef]], gameEventsLog: Vector[Event]): Receive = {
    case event: Event =>
      observers.getOrElse(event.id, Seq.empty).foreach(_ ! event)
      val updatedEventsLog = event.id match {
        case _: Event.Game => gameEventsLog :+ event
        case _             => gameEventsLog //system events are not stored
      }

      context.become(track(observers, updatedEventsLog))

    case attach: AttachEventsObserver =>
      val updatedObservers = attach.events.foldLeft(observers) {
        case (currentObservers, event) =>
          currentObservers + (event -> (currentObservers.getOrElse(event, Seq.empty) :+ attach.actor))
      }

      attach.actor ! EventsObserverAttached(attach.events: _*)
      context.become(track(updatedObservers, gameEventsLog))

    case detach: DetachEventsObserver =>
      val (detachedEvents, updatedObservers) = detach.events.foldLeft((Seq.empty[Event.Identifier], observers)) {
        case ((currentDetachedEvents, currentObservers), event) =>
          val eventObservers = observers.getOrElse(event, Seq.empty)
          val filteredEventObservers = eventObservers.filterNot(_ == detach.actor)

          val updatedDetachedEvents = if (eventObservers.size != filteredEventObservers.size) {
            currentDetachedEvents :+ event
          } else {
            currentDetachedEvents
          }

          val updatedObservers = if (filteredEventObservers.isEmpty) {
            currentObservers - event
          } else {
            currentObservers + (event -> filteredEventObservers)
          }

          (updatedDetachedEvents, updatedObservers)
      }

      detach.actor ! EventsObserverDetached(detachedEvents: _*)
      context.become(track(updatedObservers, gameEventsLog))

    case GetGameEventsLog() =>
      sender ! gameEventsLog

    case ClearGameEventsLog() =>
      context.become(track(observers, Vector.empty))
  }

  override def receive: Receive = track(observers = Map.empty, gameEventsLog = Vector.empty)
}

object Tracker {
  case class AttachEventsObserver(actor: ActorRef, events: Event.Identifier*)
  case class DetachEventsObserver(actor: ActorRef, events: Event.Identifier*)
  case class GetGameEventsLog()
  case class ClearGameEventsLog()
  case class EventsObserverAttached(events: Event.Identifier*)
  case class EventsObserverDetached(events: Event.Identifier*)

  def props(): Props = Props(classOf[Tracker])
}
