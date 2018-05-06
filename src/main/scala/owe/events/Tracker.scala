package owe.events

import akka.actor.{Actor, ActorRef, Props}
import owe.events.Tracker.{AttachEventsObserver, ClearGameEventsLog, DetachEventsObserver, GetGameEventsLog}

//TODO - use state instead of vars
class Tracker() extends Actor {
  private var observers: Map[Event.Identifier, Seq[ActorRef]] = Map.empty
  private var gameEventsLog: Vector[Event.Game] = Vector.empty

  override def receive: Receive = {
    case event: Event =>
      observers.getOrElse(event.id, Seq.empty).foreach(_ ! event)
      event match {
        case event: Event.Game => gameEventsLog :+= event
        case _                 => () //system events are not stored
      }

    case attach: AttachEventsObserver =>
      attach.events.foreach { event =>
        observers += event -> (observers.getOrElse(event, Seq.empty) :+ attach.actor)
      }

    case detach: DetachEventsObserver =>
      detach.events.foreach { event =>
        observers += event -> (observers.getOrElse(event, Seq.empty) :+ detach.actor)
      }

    case GetGameEventsLog() =>
      sender() ! gameEventsLog

    case ClearGameEventsLog() =>
      gameEventsLog = Vector.empty
  }
}

object Tracker {
  case class AttachEventsObserver(actor: ActorRef, events: Event.Identifier*)
  case class DetachEventsObserver(actor: ActorRef, events: Event.Identifier*)
  case class GetGameEventsLog()
  case class ClearGameEventsLog()

  def props(): Props = Props(classOf[Tracker])
}
