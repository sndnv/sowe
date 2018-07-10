package owe.entities.active.behaviour.resource.transformations

import owe.entities.ActiveEntity.ResourceData
import owe.entities.Entity
import owe.entities.Entity.ProcessCommodities
import owe.entities.active.Resource.State
import owe.production.Commodity

trait ProcessedUpdateMessages {
  def withProcessedUpdateMessages(resource: ResourceData, pendingMessages: Seq[Entity.Message]): State =
    pendingMessages.foldLeft(resource.state) {
      case (currentState, message) =>
        message match {
          case ProcessCommodities(commodities) if currentState.currentAmount > Commodity.Amount(0) =>
            commodities.toMap.get(resource.properties.commodity) match {
              case Some(commodity) if commodity < Commodity.Amount(0) =>
                val updatedAmount = (currentState.currentAmount + commodity).max(Commodity.Amount(0))
                currentState.copy(currentAmount = updatedAmount)

              case _ => currentState
            }

          case _ => currentState
        }
    }
}
