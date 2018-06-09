package owe.entities.active.behaviour

import akka.actor.ActorRef
import owe.EntityID
import owe.Tagging.@@
import owe.entities.ActiveEntity.{ActorRefTag, ForwardMessage}
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Exchange.{CommodityAvailable, CommodityInTransit, CommodityRequired, UpdateCommodityState}
import owe.production.{Commodity, CommodityAmount, CommodityState, Exchange}

object UpdateExchange {
  object State {
    def apply(
      commodities: Map[Commodity, CommodityAmount],
      state: CommodityState
    )(implicit parentEntity: ActorRef @@ ActorRefTag): Unit =
      forwardMessages(
        commodities.map {
          case (commodity, amount) if amount > CommodityAmount(0) =>
            UpdateCommodityState(commodity, amount, state)
        }.toSeq
      )
  }

  object Stats {
    def availableCommodities(
      entityID: EntityID,
      commodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActorRef @@ ActorRefTag): Unit =
      forwardMessages(
        commodities.map {
          case (commodity, updatedAmount) if updatedAmount > CommodityAmount(0) =>
            CommodityAvailable(commodity, updatedAmount, entityID)
        }.toSeq
      )

    def availableCommodities(
      entityID: EntityID,
      initialCommodities: Map[Commodity, CommodityAmount],
      updatedCommodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActorRef @@ ActorRefTag): Unit =
      forwardMessages(
        updatedCommodities.flatMap {
          case (commodity, updatedAmount) if updatedAmount > CommodityAmount(0) =>
            val currentAmount = initialCommodities.getOrElse(commodity, CommodityAmount(0))

            if (currentAmount != updatedAmount) {
              Some(CommodityAvailable(commodity, updatedAmount, entityID))
            } else {
              None
            }
        }.toSeq
      )

    def requiredCommodities(
      entityID: EntityID,
      consumedCommodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActorRef @@ ActorRefTag): Unit =
      forwardMessages(
        consumedCommodities.map {
          case (commodity, amount) if amount > CommodityAmount(0) =>
            CommodityRequired(commodity, amount, entityID)
        }.toSeq
      )

    def inTransitCommodities(
      source: EntityID,
      destination: EntityID,
      inTransitCommodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActorRef @@ ActorRefTag): Unit =
      forwardMessages(
        inTransitCommodities.map {
          case (commodity, amount) if amount > CommodityAmount(0) =>
            CommodityInTransit(commodity, amount, source, destination)
        }.toSeq
      )
  }

  private def forwardMessages(
    messages: Seq[Exchange.Message]
  )(implicit parentEntity: ActorRef @@ ActorRefTag): Unit =
    messages.foreach { message =>
      parentEntity ! ForwardMessage(ForwardExchangeMessage(message))
    }
}
