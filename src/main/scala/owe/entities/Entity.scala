package owe.entities

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import owe.EntityDesirability
import owe.Tagging.@@
import owe.entities.active.AttackDamage
import owe.map.grid.Point
import owe.production.{Commodity, CommodityAmount}

trait Entity[T <: Entity.ActorRefTag] {
  type Tag = T

  def `size`: Entity.Size
  def `type`: Entity.Type
  def `desirability`: EntityDesirability

  def props()(implicit timeout: Timeout): Props
}

object Entity {
  type EntityActorRef = ActorRef @@ ActorRefTag
  trait ActorRefTag

  trait Properties {
    def homePosition: Point
  }

  trait State

  trait StateModifiers

  case class Size(height: Int, width: Int)

  sealed trait Type
  object Type {
    case object Road extends Type
    case object Roadblock extends Type
    case object Doodad extends Type
    case object Resource extends Type
    case object Structure extends Type
    case object Walker extends Type
  }

  sealed trait Message extends owe.Message
  case class ProcessAttack(damage: AttackDamage) extends Message
  case class ProcessCommodities(commodities: Seq[(Commodity, CommodityAmount)]) extends Message
  case class ProcessLabourFound() extends Message
  case class ProcessOccupantsUpdate(occupants: Int) extends Message
  case class ProcessLabourUpdate(employees: Int) extends Message
}
