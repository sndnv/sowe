package owe.entities.active.behaviour

import owe.entities.ActiveEntity.{ActiveEntityRef, ForwardMessage}
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Exchange.{CommodityAvailable, CommodityInTransit, CommodityRequired, UpdateCommodityState}
import owe.production.{Commodity, CommodityAmount, CommodityState, Exchange}

object UpdateExchange {
  object State {
    def apply(
      commodities: Map[Commodity, CommodityAmount],
      state: CommodityState
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        commodities.flatMap {
          case (commodity, amount) if amount > CommodityAmount(0) =>
            Some(UpdateCommodityState(commodity, amount, state))

          case _ => None
        }.toSeq
      )
  }

  object Stats {
    def availableCommodities(
      entityID: ActiveEntityRef,
      commodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        commodities.flatMap {
          case (commodity, updatedAmount) if updatedAmount > CommodityAmount(0) =>
            Some(CommodityAvailable(commodity, updatedAmount, entityID))

          case _ => None
        }.toSeq
      )

    def availableCommodities(
      entityID: ActiveEntityRef,
      initialCommodities: Map[Commodity, CommodityAmount],
      updatedCommodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        updatedCommodities.flatMap {
          case (commodity, updatedAmount) if updatedAmount > CommodityAmount(0) =>
            val currentAmount = initialCommodities.getOrElse(commodity, CommodityAmount(0))

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
      consumedCommodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        consumedCommodities.flatMap {
          case (commodity, amount) if amount > CommodityAmount(0) =>
            Some(CommodityRequired(commodity, amount, entityID))

          case _ => None
        }.toSeq
      )

    def inTransitCommodities(
      source: ActiveEntityRef,
      destination: ActiveEntityRef,
      inTransitCommodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActiveEntityRef): Unit =
      forwardMessages(
        inTransitCommodities.flatMap {
          case (commodity, amount) if amount > CommodityAmount(0) =>
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
