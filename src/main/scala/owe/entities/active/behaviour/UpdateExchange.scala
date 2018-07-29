package owe.entities.active.behaviour

import owe.entities.ActiveEntity.ActiveEntityRef
import owe.entities.ActiveEntityActor.ForwardMessage
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Exchange.{CommodityAvailable, CommodityInTransit, CommodityRequired, UpdateCommodityState}
import owe.production.{Commodity, Exchange}

object UpdateExchange {
  object State {
    def apply(
      commodities: Map[Commodity, Commodity.Amount],
      state: Commodity.State
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        commodities.map {
          case (commodity, amount) => UpdateCommodityState(commodity, amount, state)
        }.toSeq
      )
  }

  object Stats {
    def availableCommodities(
      entityID: ActiveEntityRef,
      commodities: Map[Commodity, Commodity.Amount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        commodities.map {
          case (commodity, updatedAmount) => CommodityAvailable(commodity, updatedAmount, entityID)
        }.toSeq
      )

    def availableCommodities(
      entityID: ActiveEntityRef,
      initialCommodities: Map[Commodity, Commodity.Amount],
      updatedCommodities: Map[Commodity, Commodity.Amount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        updatedCommodities.flatMap {
          case (commodity, updatedAmount) =>
            val currentAmount = initialCommodities.getOrElse(commodity, Commodity.Amount(0))

            if (currentAmount != updatedAmount) {
              Some(CommodityAvailable(commodity, updatedAmount, entityID))
            } else {
              None
            }
        }.toSeq
      )

    def requiredCommodities(
      entityID: ActiveEntityRef,
      consumedCommodities: Map[Commodity, Commodity.Amount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        consumedCommodities.map {
          case (commodity, amount) => CommodityRequired(commodity, amount, entityID)
        }.toSeq
      )

    def inTransitCommodities(
      source: ActiveEntityRef,
      destination: ActiveEntityRef,
      inTransitCommodities: Map[Commodity, Commodity.Amount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        inTransitCommodities.map {
          case (commodity, amount) => CommodityInTransit(commodity, amount, source, destination)
        }.toSeq
      )
  }

  private def forwardMessages(
    messages: Seq[Exchange.Message]
  )(implicit parentEntity: ActiveEntityRef): Unit =
    messages.foreach { message =>
      parentEntity ! ForwardMessage(ForwardExchangeMessage(message))
    }
}
