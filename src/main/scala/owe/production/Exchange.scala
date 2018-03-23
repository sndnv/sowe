package owe.production

import akka.actor.Actor
import owe.entities.ActiveEntity.ActiveEntityActorRef
import owe.production.Exchange._

class Exchange() extends Actor {
  private var producers: Map[Commodity, Seq[ActiveEntityActorRef]] = Map.empty
  private var consumers: Map[Commodity, Seq[ActiveEntityActorRef]] = Map.empty

  private var required: Map[(Commodity, ActiveEntityActorRef), CommodityAmount] = Map.empty
  private var available: Map[(Commodity, ActiveEntityActorRef), CommodityAmount] = Map.empty
  private var inTransit: Map[(Commodity, ActiveEntityActorRef), (CommodityAmount, ActiveEntityActorRef)] =
    Map.empty

  private var produced: Map[Commodity, CommodityAmount] = Map.empty
  private var used: Map[Commodity, CommodityAmount] = Map.empty
  private var lost: Map[Commodity, CommodityAmount] = Map.empty

  override def receive: Receive = {
    case CommodityRequired(commodity, amount, source) =>
      required += (commodity, source) -> amount

    case CommodityAvailable(commodity, amount, source) =>
      available += (commodity, source) -> amount

    case CommodityInTransit(commodity, amount, source, destination) =>
      inTransit += (commodity, source) -> (amount, destination)

    case UpdateCommodityState(commodity, amount, state) =>
      state match {
        case CommodityState.Produced =>
          produced += commodity -> (produced.getOrElse(commodity, CommodityAmount(0)) + amount)

        case CommodityState.Used =>
          used += commodity -> (used.getOrElse(commodity, CommodityAmount(0)) + amount)

        case CommodityState.Lost =>
          lost += commodity -> (lost.getOrElse(commodity, CommodityAmount(0)) + amount)
      }

    case AddProducer(source, commodity) =>
      producers += commodity -> (producers.getOrElse(commodity, Seq.empty) :+ source).distinct

    case RemoveProducer(source, commodity) =>
      producers.get(commodity) match {
        case Some(commodityProducers) => producers += commodity -> commodityProducers.filter(_ == source)
        case None                     => () //ignore
      }

    case AddConsumer(source, commodity) =>
      consumers += commodity -> (consumers.getOrElse(commodity, Seq.empty) :+ source).distinct

    case RemoveConsumer(source, commodity) =>
      consumers.get(commodity) match {
        case Some(commodityProducers) => consumers += commodity -> commodityProducers.filter(_ == source)
        case None                     => () //ignore
      }
  }
}

object Exchange {
  sealed trait Message extends owe.Message

  case class AddProducer(source: ActiveEntityActorRef, commodity: Commodity) extends Message
  case class RemoveProducer(source: ActiveEntityActorRef, commodity: Commodity) extends Message
  case class AddConsumer(source: ActiveEntityActorRef, commodity: Commodity) extends Message
  case class RemoveConsumer(source: ActiveEntityActorRef, commodity: Commodity) extends Message

  case class CommodityRequired(
    commodity: Commodity,
    amount: CommodityAmount,
    source: ActiveEntityActorRef
  ) extends Message

  case class CommodityAvailable(
    commodity: Commodity,
    amount: CommodityAmount,
    source: ActiveEntityActorRef
  ) extends Message

  case class CommodityInTransit(
    commodity: Commodity,
    amount: CommodityAmount,
    source: ActiveEntityActorRef,
    destination: ActiveEntityActorRef
  ) extends Message

  case class UpdateCommodityState(
    commodity: Commodity,
    amount: CommodityAmount,
    state: CommodityState
  ) extends Message
}
