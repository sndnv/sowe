package owe.entities.passive

import akka.actor.ActorRef
import owe.entities.Entity.Desirability
import owe.entities.PassiveEntity.PassiveEntityRef
import owe.entities.{Entity, PassiveEntity}

class Road extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Road

  override def `desirability`: Desirability = Desirability.Neutral
}

object Road {
  case class RoadRef(ref: ActorRef) extends PassiveEntityRef
}
