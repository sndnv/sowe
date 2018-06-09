package owe.entities.active.behaviour.resource.transformations

import owe.entities.ActiveEntity.ResourceData
import owe.entities.active.Resource.State
import owe.production.CommodityAmount

trait ReplenishedResources {
  def withReplenishedResources(resource: ResourceData, amountProduced: Option[CommodityAmount]): State =
    amountProduced match {
      case Some(produced) => resource.state.copy(currentAmount = produced)
      case None           => resource.state
    }
}
