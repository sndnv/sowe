package owe.entities.active.behaviour.resource.producing

import owe.entities.ActiveEntity.{ProcessEntityTick, ResourceData}
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.resource.BaseResource.Become
import owe.entities.active.behaviour.resource.transformations.{ProcessedUpdateMessages, ReplenishedResources}
import owe.entities.active.behaviour.resource.{BaseResource, CommodityCalculations}
import owe.production.CommodityState

trait ProducingResource extends BaseResource with ProcessedUpdateMessages with ReplenishedResources {

  import context.dispatcher

  override protected def behaviour: Behaviour = producing()

  final protected def producing(): Behaviour = {
    case ProcessEntityTick(_, resource: ResourceData, messages) =>
      val amountProduced = CommodityCalculations.amountProduced(resource)

      withUpdates(
        resource,
        Seq(
          withProcessedUpdateMessages(_: ResourceData, messages),
          withReplenishedResources(_: ResourceData, amountProduced)
        )
      ).foreach { updatedData =>
        amountProduced.foreach { amountProduced =>
          UpdateExchange.State(
            Map(resource.properties.commodity -> amountProduced),
            CommodityState.Produced
          )

          UpdateExchange.Stats.availableCommodities(
            resource.properties.id,
            Map(resource.properties.commodity -> updatedData.state.currentAmount)
          )
        }

        self ! Become(() => producing(), updatedData)
      }
  }
}
