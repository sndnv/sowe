package owe.entities.active.behaviour

import akka.actor.typed.ActorRef
import owe.entities.ActiveEntity.{EntityMessage, ForwardMessage}
import owe.entities.Entity.EntityActorRef
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Exchange.{CommodityAvailable, CommodityInTransit, CommodityRequired, UpdateCommodityState}
import owe.production.{Commodity, CommodityAmount, CommodityState, Exchange}

object UpdateExchange {
  object State {
    def apply(
      commodities: Map[Commodity, CommodityAmount],
      state: CommodityState
    )(implicit parentEntity: ActorRef[EntityMessage]): Unit =
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
      entityID: EntityActorRef,
      commodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActorRef[EntityMessage]): Unit =
      forwardMessages(
        commodities.flatMap {
          case (commodity, updatedAmount) if updatedAmount > CommodityAmount(0) =>
            Some(CommodityAvailable(commodity, updatedAmount, entityID))

          case _ => None
        }.toSeq
      )

    def availableCommodities(
      entityID: EntityActorRef,
      initialCommodities: Map[Commodity, CommodityAmount],
      updatedCommodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActorRef[EntityMessage]): Unit =
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
      entityID: EntityActorRef,
      consumedCommodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActorRef[EntityMessage]): Unit =
      forwardMessages(
        consumedCommodities.flatMap {
          case (commodity, amount) if amount > CommodityAmount(0) =>
            Some(CommodityRequired(commodity, amount, entityID))

          case _ => None
        }.toSeq
      )

    def inTransitCommodities(
      source: EntityActorRef,
      destination: EntityActorRef,
      inTransitCommodities: Map[Commodity, CommodityAmount]
    )(implicit parentEntity: ActorRef[EntityMessage]): Unit =
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
  )(implicit parentEntity: ActorRef[EntityMessage]): Unit =
    messages.foreach { message =>
      parentEntity ! ForwardMessage(ForwardExchangeMessage(message))
    }
}
