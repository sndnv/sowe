package owe.test.specs.unit.entities.active.behaviour

import akka.actor.{Actor, ActorRef, Props}
import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor._
import owe.test.specs.unit.entities.active.behaviour.StructureParentEntity.MockDestroySelf

class StructureParentEntity(ref: ActorRef, childProps: Props) extends Actor {
  private val child = context.actorOf(childProps)

  override def receive: Receive = {
    case apply: ApplyInstructions   => child ! apply
    case apply: ApplyMessages       => child ! apply
    case tick: ProcessBehaviourTick => child ! tick
    case create: CreateBehaviour    => child ! create
    case MockDestroySelf(data)      => child ! DestroyBehaviour(data)
    case message                    => ref.forward(message)
  }
}

object StructureParentEntity {
  case class MockDestroySelf(data: StructureData)

  def props(ref: ActorRef, childProps: Props) = Props(classOf[StructureParentEntity], ref, childProps)
}
