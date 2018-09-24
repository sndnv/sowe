package owe.entities.passive

import akka.actor.ActorRef
import owe.entities.Entity.Desirability
import owe.entities.PassiveEntity.PassiveEntityRef
import owe.entities.{Entity, PassiveEntity}
import owe.map.Cell

class Doodad extends PassiveEntity {
  final override def `type`: Entity.Type = Entity.Type.Doodad

  override def `desirability`: Desirability = Desirability.Neutral

  override def acceptsAvailability(availability: Cell.Availability): Boolean = availability.isFree
}

object Doodad {
  case class DoodadRef(ref: ActorRef) extends PassiveEntityRef
}
