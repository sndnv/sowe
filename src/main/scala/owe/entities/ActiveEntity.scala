package owe.entities

import akka.actor.Actor

trait ActiveEntity extends Entity with Actor {}

object ActiveEntity {

  case class ProcessTick()

  case class ProcessEffects()

  case class UpdateDestination()

  case class UpdateAction()

  case class TickResult()

}
