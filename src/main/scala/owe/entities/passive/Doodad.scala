package owe.entities.passive

import akka.actor.ActorRef
import owe.EntityDesirability
import owe.entities.PassiveEntity.PassiveEntityRef
import owe.entities.{Entity, PassiveEntity}

class Doodad extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Doodad

  override def `desirability`: EntityDesirability = EntityDesirability.Neutral
}

object Doodad {
  case class DoodadRef(ref: ActorRef) extends PassiveEntityRef
}
