package owe.entities.active.behaviour.resource

import akka.actor.Actor.Receive
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.ResourceData
import owe.entities.ActiveEntityActor._
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.behaviour.{BaseBehaviour, UpdateExchange}
import owe.entities.active.behaviour.resource.BaseResource.Become
import owe.production.Commodity

trait BaseResource extends BaseBehaviour {

  final override private[behaviour] implicit val parentEntity: ActiveEntity.ActiveEntityRef =
    ResourceRef(context.parent)

  override protected def base: Behaviour = {
    case CreateBehaviour(resource: ResourceData) =>
      UpdateExchange.Producers.add(parentEntity, resource.properties.commodity)

    case DestroyBehaviour(resource: ResourceData) =>
      UpdateExchange.Stats.availableCommodities(parentEntity, Map(resource.properties.commodity -> Commodity.Amount(0)))
      UpdateExchange.Producers.remove(parentEntity, resource.properties.commodity)

      parentEntity ! BehaviourDestroyed()
      context.become(destroying())

    case Become(behaviour, resource) =>
      parentEntity ! BehaviourTickProcessed(resource.state)
      become(behaviour, resource)
  }

  protected def destroying(): Behaviour = {
    case ApplyInstructions(_, _) =>
      log.debug("Entity [{}] waiting to be destroyed; instructions application ignored", self)
      parentEntity ! InstructionsApplied()

    case ApplyMessages(resource: ResourceData, _) =>
      log.debug("Entity [{}] waiting to be destroyed; messages application ignored", self)
      parentEntity ! MessagesApplied(resource.state)

    case ProcessBehaviourTick(_, entity) =>
      log.debug("Entity [{}] waiting to be destroyed; tick ignored", self)
      parentEntity ! BehaviourTickProcessed(entity.state)
  }

  private def become(behaviour: () => Behaviour, resource: ResourceData): Unit =
    context.become(base.orElse(behaviour()))
}

object BaseResource {
  private[behaviour] case class Become(behaviour: () => Receive, resource: ResourceData)
}
