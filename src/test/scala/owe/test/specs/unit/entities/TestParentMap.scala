package owe.test.specs.unit.entities

import akka.actor.{Actor, ActorRef, Props}
import owe.map.GameMap
import TestParentMap._

class TestParentMap(ref: ActorRef, entityProps: Props) extends Actor {

  private val entityActor = context.actorOf(entityProps)

  override def receive: Receive = {
    case _: GetEntity             => sender ! entityActor
    case message: GameMap.Message => ref.forward(ParentMapMessage(message))
  }
}

object TestParentMap {
  case class GetEntity()
  case class ParentMapMessage(message: GameMap.Message)
}
