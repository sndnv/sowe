package owe.entities

import akka.actor.{Actor, ActorLogging}

import scala.util.Failure

class PassiveEntityActor extends Actor with ActorLogging {
  override def receive: Receive = {
    case message: owe.Message =>
      val errorMessage = s"Passive entity received message [$message]!"
      log.error(errorMessage)
      sender ! Failure(new IllegalStateException(errorMessage))
  }
}
