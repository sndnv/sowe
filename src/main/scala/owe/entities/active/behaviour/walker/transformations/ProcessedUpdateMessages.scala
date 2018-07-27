package owe.entities.active.behaviour.walker.transformations

import owe.entities.ActiveEntity.WalkerData
import owe.entities.Entity
import owe.entities.Entity.{ProcessAttack, ProcessCommodities, ProcessMovement}
import owe.entities.active.Walker.{CommoditiesState, NoCommodities, State}

import scala.concurrent.Future

trait ProcessedUpdateMessages {
  def withProcessedUpdateMessages(walker: WalkerData, pendingMessages: Seq[Entity.Message]): Future[State] =
    Future.successful(
      pendingMessages.foldLeft(walker.state) {
        case (currentState, message) =>
          message match {
            case ProcessMovement(updatedPosition) =>
              currentState.copy(
                currentPosition = updatedPosition
              )

            case ProcessAttack(damage) =>
              currentState.copy(currentLife = damage(currentState.currentLife))

            case ProcessCommodities(commodities) =>
              currentState.commodities match {
                case CommoditiesState(available, limits) =>
                  currentState.copy(
                    commodities = CommoditiesState(
                      available.mergeWithLimits(commodities.toMap, limits),
                      limits
                    )
                  )

                case NoCommodities =>
                  currentState
              }

            case _ => currentState
          }
      }
    )
}
