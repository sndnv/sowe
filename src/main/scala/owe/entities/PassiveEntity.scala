package owe.entities

import akka.NotUsed
import akka.actor.typed.ActorRef
import akka.actor.{Actor, ActorLogging, Props}
import akka.util.Timeout

// TODO - fix
trait PassiveEntity extends Entity {
  override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)

  override def props()(implicit timeout: Timeout): Props = Props(
    classOf[PassiveEntityActor]
  )

  private class PassiveEntityActor extends Actor with ActorLogging {
    // TODO - ?
    override def receive: Receive = {
      case message: owe.Message =>
        log.error(s"Passive entity received message [$message]!")
    }
  }
}

object PassiveEntity {
  trait PassiveEntityActorRef extends ActorRef[NotUsed]
}
