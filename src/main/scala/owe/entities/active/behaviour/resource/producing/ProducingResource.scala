package owe.entities.active.behaviour.resource.producing

import scala.concurrent.ExecutionContext

import akka.actor.typed.scaladsl.Behaviors
import owe.entities.ActiveEntity._
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.resource.BaseResource.Become
import owe.entities.active.behaviour.resource.transformations.{ProcessedUpdateMessages, ReplenishedResources}
import owe.entities.active.behaviour.resource.{BaseResource, CommodityCalculations}
import owe.production.CommodityState

trait ProducingResource extends BaseResource with ProcessedUpdateMessages with ReplenishedResources {

  override protected def behaviour(implicit ec: ExecutionContext): Behaviour = producing()

  final protected def producing(): Behaviour = Behaviors.receive[EntityBehaviourMessage] { (ctx, msg) =>
    import ctx.executionContext

    msg match {
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
              resource.id,
              Map(resource.properties.commodity -> updatedData.state.currentAmount)
            )
          }

          ctx.self ! Become(() => producing(), updatedData)
        }

        Behaviors.same // TODO
    }
  }
}
