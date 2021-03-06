package owe.entities.active.behaviour.resource

import owe.entities.ActiveEntity.ResourceData
import owe.production.Commodity

object CommodityCalculations {
  def amountProduced(resource: ResourceData): Option[Commodity.Amount] =
    if (resource.state.currentAmount >= resource.properties.maxAmount) {
      None
    } else {
      val replenishAmount = resource.modifiers.replenishAmount(resource.state.replenishAmount)
      val newAmount = resource.state.currentAmount + replenishAmount
      Some(
        resource.properties.maxAmount.min(newAmount) - resource.state.currentAmount
      )
    }
}
