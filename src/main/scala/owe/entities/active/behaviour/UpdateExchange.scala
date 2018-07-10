package owe.entities.active.behaviour

import owe.entities.ActiveEntity.{ActiveEntityRef, ForwardMessage}
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
        commodities.flatMap {
          case (commodity, amount) if amount > Commodity.Amount(0) =>
            Some(UpdateCommodityState(commodity, amount, state))

          case _ => None
        }.toSeq
      )
  }

  object Stats {
    def availableCommodities(
      entityID: ActiveEntityRef,
      commodities: Map[Commodity, Commodity.Amount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        commodities.flatMap {
          case (commodity, updatedAmount) if updatedAmount > Commodity.Amount(0) =>
            Some(CommodityAvailable(commodity, updatedAmount, entityID))

          case _ => None
        }.toSeq
      )

    def availableCommodities(
      entityID: ActiveEntityRef,
      initialCommodities: Map[Commodity, Commodity.Amount],
      updatedCommodities: Map[Commodity, Commodity.Amount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        updatedCommodities.flatMap {
          case (commodity, updatedAmount) if updatedAmount > Commodity.Amount(0) =>
            val currentAmount = initialCommodities.getOrElse(commodity, Commodity.Amount(0))

            if (currentAmount != updatedAmount) {
              Some(CommodityAvailable(commodity, updatedAmount, entityID))
            } else {
              None
            }

          case _ => None
        }.toSeq
      )

    def requiredCommodities(
      entityID: ActiveEntityRef,
      consumedCommodities: Map[Commodity, Commodity.Amount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        consumedCommodities.flatMap {
          case (commodity, amount) if amount > Commodity.Amount(0) =>
            Some(CommodityRequired(commodity, amount, entityID))

          case _ => None
        }.toSeq
      )

    def inTransitCommodities(
      source: ActiveEntityRef,
      destination: ActiveEntityRef,
      inTransitCommodities: Map[Commodity, Commodity.Amount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        inTransitCommodities.flatMap {
          case (commodity, amount) if amount > Commodity.Amount(0) =>
            Some(CommodityInTransit(commodity, amount, source, destination))

          case _ => None
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
