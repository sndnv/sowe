package owe.test.specs.unit.entities.active.behaviour

import akka.actor.{Actor, ActorRef, Props}
import owe.entities.ActiveEntityActor.ProcessEntityTick

class ForwardingParentEntity(ref: ActorRef, childProps: Props) extends Actor {
  private val child = context.actorOf(childProps)

  override def receive: Receive = {
    case tick: ProcessEntityTick => child ! tick
    case message                 => ref.forward(message)
  }
}

object ForwardingParentEntity {
  def props(ref: ActorRef, childProps: Props) = Props(classOf[ForwardingParentEntity], ref, childProps)
}
