package owe.entities

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import owe.entities.Entity.Desirability
import owe.entities.active.attributes.AttackDamage
import owe.map.Cell
import owe.map.grid.Point
import owe.production.Commodity

import scala.concurrent.Future

trait Entity {
  def `size`: Entity.Size
  def `type`: Entity.Type
  def `desirability`: Desirability

  def props()(implicit timeout: Timeout): Props
}

object Entity {
  trait EntityRef {
    def ref: ActorRef
    def !(message: Any)(implicit sender: ActorRef = Actor.noSender): Unit = ref ! message
    def ?(message: Any)(implicit timeout: Timeout, sender: ActorRef): Future[Any] = ref ? message
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
  case class ProcessCommodities(commodities: Seq[(Commodity, Commodity.Amount)]) extends Message
  case class ProcessMovement(updatedPosition: Point) extends Message
  case class ProcessLabourFound() extends Message
  case class ProcessOccupantsUpdate(occupants: Int) extends Message
  case class ProcessLabourUpdate(employees: Int) extends Message

  def cells(entitySize: Entity.Size, parentCell: Point): Seq[Point] =
    (parentCell.x until (parentCell.x + entitySize.width))
      .flatMap(
        x => (parentCell.y until (parentCell.y + entitySize.height)).map(y => Point(x, y))
      )

  final case class Desirability(
    self: Cell.Desirability,
    r1: Cell.Desirability,
    r2: Cell.Desirability,
    r3: Cell.Desirability,
    r4: Cell.Desirability,
    r5: Cell.Desirability
  ) {
    def +(that: Desirability): Desirability =
      Desirability(
        this.self + that.self,
        this.r1 + that.r1,
        this.r2 + that.r2,
        this.r3 + that.r3,
        this.r4 + that.r4,
        this.r5 + that.r5
      )

    def -(that: Desirability): Desirability =
      Desirability(
        this.self - that.self,
        this.r1 - that.r1,
        this.r2 - that.r2,
        this.r3 - that.r3,
        this.r4 - that.r4,
        this.r5 - that.r5
      )

    def toMap: Map[Int, Cell.Desirability] = Map(
      0 -> self,
      1 -> r1,
      2 -> r2,
      3 -> r3,
      4 -> r4,
      5 -> r5
    )
  }

  object Desirability {
    def fromInt(
      self: Int,
      r1: Int,
      r2: Int,
      r3: Int,
      r4: Int,
      r5: Int
    ): Desirability = new Desirability(
      Cell.Desirability(self),
      Cell.Desirability(r1),
      Cell.Desirability(r2),
      Cell.Desirability(r3),
      Cell.Desirability(r4),
      Cell.Desirability(r5)
    )

    val Min: Desirability = Desirability(
      Cell.Desirability.Min,
      Cell.Desirability.Min,
      Cell.Desirability.Min,
      Cell.Desirability.Min,
      Cell.Desirability.Min,
      Cell.Desirability.Min
    )

    val Max: Desirability = Desirability(
      Cell.Desirability.Max,
      Cell.Desirability.Max,
      Cell.Desirability.Max,
      Cell.Desirability.Max,
      Cell.Desirability.Max,
      Cell.Desirability.Max
    )

    val Neutral: Desirability = Desirability(
      Cell.Desirability.Neutral,
      Cell.Desirability.Neutral,
      Cell.Desirability.Neutral,
      Cell.Desirability.Neutral,
      Cell.Desirability.Neutral,
      Cell.Desirability.Neutral
    )
  }
}
