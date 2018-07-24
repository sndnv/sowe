package owe.entities.active.behaviour.resource.producing

import owe.entities.ActiveEntity.ResourceData
import owe.entities.ActiveEntityActor.{ApplyMessages, MessagesApplied, ProcessBehaviourTick}
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.resource.BaseResource.Become
import owe.entities.active.behaviour.resource.transformations.{ProcessedUpdateMessages, ReplenishedResources}
import owe.entities.active.behaviour.resource.{BaseResource, CommodityCalculations}
import owe.production.Commodity

trait ProducingResource extends BaseResource with ProcessedUpdateMessages with ReplenishedResources {

  import context.dispatcher

  override protected def behaviour: Behaviour = producing()

  final protected def producing(): Behaviour = {
    case ApplyMessages(resource: ResourceData, messages) =>
      log.debug("Applying [{}] messages: [{}]", messages.size, messages)

      withUpdates(
        resource,
        Seq(
          withProcessedUpdateMessages(_: ResourceData, messages)
        )
      ).foreach { updatedData =>
        parentEntity ! MessagesApplied(updatedData.state)
      }

    case ProcessBehaviourTick(_, resource: ResourceData) =>
      log.debug("Processing behaviour tick as [producing] with data [{}]", resource)

      val amountProduced = CommodityCalculations.amountProduced(resource)

      withUpdates(
        resource,
        Seq(
          withReplenishedResources(_: ResourceData, amountProduced)
        )
      ).foreach { updatedData =>
        amountProduced.foreach { amountProduced =>
          UpdateExchange.State(
            Map(resource.properties.commodity -> amountProduced),
            Commodity.State.Produced
          )

          UpdateExchange.Stats.availableCommodities(
            resource.id,
            Map(resource.properties.commodity -> updatedData.state.currentAmount)
          )
        }

        self ! Become(() => producing(), updatedData)
      }
  }
}
