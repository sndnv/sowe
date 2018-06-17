package owe.entities

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.Timeout
import owe.Tagging.@@

trait PassiveEntity[T <: PassiveEntity.ActorRefTag] extends Entity[T] {
  override type Tag = T

  override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)

  override def props()(implicit timeout: Timeout): Props = Props(
    classOf[PassiveEntityActor]
  )

  private class PassiveEntityActor extends Actor with ActorLogging {
    override def receive: Receive = {
      case message: owe.Message =>
        log.error(s"Passive entity received message [$message]!")
    }
  }
}

object PassiveEntity {
  type PassiveEntityActorRef = ActorRef @@ ActorRefTag

  trait ActorRefTag extends Entity.ActorRefTag
}
