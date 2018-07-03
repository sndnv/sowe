package owe.entities

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import owe.EntityDesirability
import owe.entities.active.AttackDamage
import owe.map.grid.Point
import owe.production.{Commodity, CommodityAmount}

import scala.concurrent.Future

trait Entity {
  def `size`: Entity.Size
  def `type`: Entity.Type
  def `desirability`: EntityDesirability

  def props()(implicit timeout: Timeout): Props
}

object Entity {
  trait EntityRef {
    def ref: ActorRef
    def !(message: Any): Unit = ref ! message
    def ?(message: Any)(implicit timeout: Timeout): Future[Any] = ref ? message
  }

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

  def cells(entitySize: Entity.Size, parentCell: Point): Seq[Point] =
    (parentCell.x to entitySize.width)
      .flatMap(
        x => (parentCell.y to entitySize.height).map(y => Point(x, y))
      )
}
