package owe.test.specs.unit.entities.active.behaviour

import akka.actor.{Actor, ActorRef, Props}
import owe.entities.ActiveEntityActor.{ApplyInstructions, ApplyMessages, ProcessBehaviourTick}

class StructureParentEntity(ref: ActorRef, childProps: Props) extends Actor {
  private val child = context.actorOf(childProps)

  override def receive: Receive = {
    case apply: ApplyInstructions   => child ! apply
    case apply: ApplyMessages       => child ! apply
    case tick: ProcessBehaviourTick => child ! tick
    case message                    => ref.forward(message)
  }
}

object StructureParentEntity {
  def props(ref: ActorRef, childProps: Props) = Props(classOf[StructureParentEntity], ref, childProps)
}
