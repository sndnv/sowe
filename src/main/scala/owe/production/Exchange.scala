package owe.production

import akka.actor.{Actor, Props}
import owe.entities.Entity.EntityActorRef
import owe.production.Exchange._

class Exchange() extends Actor {
  private def handler(
    entities: ExchangeEntities,
    commodities: ExchangeCommodities,
    stats: ExchangeStats
  ): Receive = {
    case CommodityRequired(commodity, amount, source) =>
      val updatedCommodities = commodities.copy(
        required = commodities.required + ((commodity, source) -> amount)
      )

      context.become(handler(entities, updatedCommodities, stats))

    case CommodityAvailable(commodity, amount, source) =>
      val updatedCommodities = commodities.copy(
        available = commodities.available + ((commodity, source) -> amount)
      )

      context.become(handler(entities, updatedCommodities, stats))

    case CommodityInTransit(commodity, amount, source, destination) =>
      val updatedCommodities = commodities.copy(
        inTransit = commodities.inTransit + ((commodity, source) -> (amount, destination))
      )

      context.become(handler(entities, updatedCommodities, stats))

    case UpdateCommodityState(commodity, amount, state) =>
      val updatedStats = state match {
        case CommodityState.Produced =>
          stats.copy(
            produced = stats.produced + (commodity -> (stats.produced
              .getOrElse(commodity, CommodityAmount(0)) + amount))
          )

        case CommodityState.Used =>
          stats.copy(
            used = stats.used + (commodity -> (stats.used.getOrElse(commodity, CommodityAmount(0)) + amount))
          )

        case CommodityState.Lost =>
          stats.copy(
            lost = stats.lost + (commodity -> (stats.lost.getOrElse(commodity, CommodityAmount(0)) + amount))
          )
      }

      context.become(handler(entities, commodities, updatedStats))

    case AddProducer(source, commodity) =>
      val updatedProducers = entities.producers + (commodity -> (entities.producers.getOrElse(commodity, Seq.empty) :+ source).distinct)

      context.become(handler(entities.copy(producers = updatedProducers), commodities, stats))

    case RemoveProducer(source, commodity) =>
      entities.producers.get(commodity).foreach { commodityProducers =>
        val filteredProducers = commodityProducers.filter(_ != source)
        val updatedProducers = if (filteredProducers.nonEmpty) {
          entities.producers + (commodity -> filteredProducers)
        } else {
          entities.producers - commodity
        }

        context.become(handler(entities.copy(producers = updatedProducers), commodities, stats))
      }

    case AddConsumer(source, commodity) =>
      val updatedConsumers = entities.consumers + (commodity -> (entities.consumers.getOrElse(commodity, Seq.empty) :+ source).distinct)

      context.become(handler(entities.copy(consumers = updatedConsumers), commodities, stats))

    case RemoveConsumer(source, commodity) =>
      entities.consumers.get(commodity).foreach { commodityConsumers =>
        val filteredConsumers = commodityConsumers.filter(_ != source)
        val updatedConsumers = if (filteredConsumers.nonEmpty) {
          entities.consumers + (commodity -> filteredConsumers)
        } else {
          entities.consumers - commodity
        }

        context.become(handler(entities.copy(consumers = updatedConsumers), commodities, stats))
      }

    case GetExchangeCommodities() =>
      sender ! ExchangeCommodities(commodities.required, commodities.available, commodities.inTransit)

    case GetExchangeStats() =>
      sender ! ExchangeStats(stats.produced, stats.used, stats.lost)

    case GetExchangeEntities() =>
      sender ! ExchangeEntities(entities.producers, entities.consumers)
  }

  override def receive: Receive = handler(
    entities = ExchangeEntities.empty,
    commodities = ExchangeCommodities.empty,
    stats = ExchangeStats.empty
  )
}

object Exchange {
  sealed trait Message extends owe.Message

  case class AddProducer(source: EntityActorRef, commodity: Commodity) extends Message
  case class RemoveProducer(source: EntityActorRef, commodity: Commodity) extends Message
  case class AddConsumer(source: EntityActorRef, commodity: Commodity) extends Message
  case class RemoveConsumer(source: EntityActorRef, commodity: Commodity) extends Message

  case class GetExchangeEntities() extends Message
  case class GetExchangeCommodities() extends Message
  case class GetExchangeStats() extends Message

  case class ExchangeEntities(
    producers: Map[Commodity, Seq[EntityActorRef]],
    consumers: Map[Commodity, Seq[EntityActorRef]]
  )

  object ExchangeEntities {
    def empty: ExchangeEntities = ExchangeEntities(
      producers = Map.empty,
      consumers = Map.empty
    )
  }

  case class ExchangeCommodities(
    required: Map[(Commodity, EntityActorRef), CommodityAmount],
    available: Map[(Commodity, EntityActorRef), CommodityAmount],
    inTransit: Map[(Commodity, EntityActorRef), (CommodityAmount, EntityActorRef)]
  )

  object ExchangeCommodities {
    def empty: ExchangeCommodities = ExchangeCommodities(
      required = Map.empty,
      available = Map.empty,
      inTransit = Map.empty
    )
  }

  case class ExchangeStats(
    produced: Map[Commodity, CommodityAmount],
    used: Map[Commodity, CommodityAmount],
    lost: Map[Commodity, CommodityAmount]
  )

  object ExchangeStats {
    def empty: ExchangeStats = ExchangeStats(
      produced = Map.empty,
      used = Map.empty,
      lost = Map.empty
    )
  }

  case class CommodityRequired(
    commodity: Commodity,
    amount: CommodityAmount,
    source: EntityActorRef
  ) extends Message

  case class CommodityAvailable(
    commodity: Commodity,
    amount: CommodityAmount,
    source: EntityActorRef
  ) extends Message

  case class CommodityInTransit(
    commodity: Commodity,
    amount: CommodityAmount,
    source: EntityActorRef,
    destination: EntityActorRef
  ) extends Message

  case class UpdateCommodityState(
    commodity: Commodity,
    amount: CommodityAmount,
    state: CommodityState
  ) extends Message

  def props(): Props = Props(classOf[Exchange])
}
