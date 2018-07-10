package owe.entities.active.behaviour.walker.transformations

import owe.entities.ActiveEntity.WalkerData
import owe.entities.Entity
import owe.entities.Entity.{ProcessAttack, ProcessCommodities}
import owe.entities.active.Walker.{CommoditiesState, NoCommodities, State}
import owe.production.Commodity

import scala.concurrent.Future

trait ProcessedUpdateMessages {
  def withProcessedUpdateMessages(walker: WalkerData, pendingMessages: Seq[Entity.Message]): Future[State] =
    Future.successful(
      pendingMessages.foldLeft(walker.state) {
        case (currentState, message) =>
          message match {
            case ProcessAttack(damage) =>
              currentState.copy(currentLife = damage(currentState.currentLife))

            case ProcessCommodities(commodities) =>
              currentState.commodities match {
                case CommoditiesState(available, limits) =>
                  val updatedCommodities = available ++ commodities.map {
                    case (commodity, amount) =>
                      val limitAmount = limits.getOrElse(commodity, Commodity.Amount(0))
                      val updatedCommodityAmount =
                        (available.getOrElse(commodity, Commodity.Amount(0)) + amount)
                          .min(limitAmount)
                          .max(Commodity.Amount(0))

                      (commodity, updatedCommodityAmount)
                  }
                  currentState.copy(commodities = CommoditiesState(updatedCommodities, limits))

                case NoCommodities =>
                  currentState
              }

            case _ => currentState
          }
      }
    )
}
