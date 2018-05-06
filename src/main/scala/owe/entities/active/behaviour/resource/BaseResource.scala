package owe.entities.active.behaviour.resource

import akka.actor.Actor.Receive
import owe.entities.ActiveEntity.{ForwardMessage, ProcessEntityTick, ResourceData}
import owe.entities.Entity
import owe.entities.Entity.ProcessCommodities
import owe.entities.active.Resource
import owe.entities.active.Resource._
import owe.entities.active.behaviour.BaseBehaviour
import owe.entities.active.behaviour.resource.BaseResource.Become
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Exchange.{CommodityAvailable, UpdateCommodityState}
import owe.production.{CommodityAmount, CommodityState}

import scala.concurrent.Future

trait BaseResource extends BaseBehaviour[Resource.ActorRefTag] {
  import context.dispatcher

  override protected def behaviour: Behaviour = producing()

  final protected def producing(): Behaviour = {
    case ProcessEntityTick(_, resource: ResourceData, messages) =>
      withUpdates(
        resource,
        Seq(
          withProcessedUpdateMessages(_: ResourceData, messages),
          withReplenishedResources(_: ResourceData)
        )
      ).foreach { updatedData =>
        calculateAmountProduced(resource)
          .foreach { amountProduced =>
            parent ! ForwardMessage(
              ForwardExchangeMessage(
                UpdateCommodityState(resource.properties.commodity, amountProduced, CommodityState.Produced)
              )
            )
          }

        if (resource.state.currentAmount != updatedData.state.currentAmount) {
          parent ! ForwardMessage(
            ForwardExchangeMessage(
              CommodityAvailable(
                resource.properties.commodity,
                updatedData.state.currentAmount,
                resource.properties.id
              )
            )
          )
        }

        self ! Become(producing, updatedData)
      }
  }

  private def withReplenishedResources(resource: ResourceData): Future[State] =
    Future.successful(
      calculateAmountProduced(resource) match {
        case Some(amountProduced) => resource.state.copy(currentAmount = amountProduced)
        case None                 => resource.state
      }
    )

  private def withProcessedUpdateMessages(resource: ResourceData, pendingMessages: Seq[Entity.Message]): Future[State] =
    Future.successful(
      pendingMessages.foldLeft(resource.state) {
        case (currentState, message) =>
          message match {
            case ProcessCommodities(commodities) if resource.state.currentAmount > CommodityAmount(0) =>
              commodities.toMap.get(resource.properties.commodity) match {
                case Some(commodity) if commodity > CommodityAmount(0) =>
                  val updatedAmount = (resource.state.currentAmount - commodity).max(CommodityAmount(0))
                  currentState.copy(currentAmount = updatedAmount)

                case None => currentState
              }

            case _ => currentState
          }
      }
    )

  private def calculateAmountProduced(resource: ResourceData): Option[CommodityAmount] =
    if (resource.state.currentAmount == resource.properties.maxAmount) {
      None
    } else {
      val replenishAmount = resource.modifiers.replenishAmount(resource.state.replenishAmount)
      val newAmount = resource.state.currentAmount + replenishAmount
      Some(resource.properties.maxAmount.min(newAmount))
    }

  private def base(): Behaviour = {
    case Become(behaviour, resource) =>
      parent ! resource.state
      become(behaviour, resource)
  }

  private def become(behaviour: () => Behaviour, resource: ResourceData): Unit =
    context.become(base().orElse(behaviour()))
}

object BaseResource {
  private case class Become(behaviour: () => Receive, resource: ResourceData)
}
