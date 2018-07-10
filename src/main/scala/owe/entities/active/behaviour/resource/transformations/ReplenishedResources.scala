package owe.entities.active.behaviour.resource.transformations

import owe.entities.ActiveEntity.ResourceData
import owe.entities.active.Resource.State
import owe.production.Commodity

trait ReplenishedResources {
  def withReplenishedResources(resource: ResourceData, amountProduced: Option[Commodity.Amount]): State =
    amountProduced match {
      case Some(produced) => resource.state.copy(currentAmount = resource.state.currentAmount + produced)
      case None           => resource.state
    }
}
