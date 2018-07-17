package owe.entities.active.behaviour.resource

import akka.actor.Actor.Receive
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.ResourceData
import owe.entities.ActiveEntityActor.BehaviourTickProcessed
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.behaviour.BaseBehaviour
import owe.entities.active.behaviour.resource.BaseResource.Become

trait BaseResource extends BaseBehaviour {

  final override private[behaviour] implicit val parentEntity: ActiveEntity.ActiveEntityRef =
    ResourceRef(context.parent)

  override protected def base: Behaviour = {
    case Become(behaviour, tick, resource) =>
      parentEntity ! BehaviourTickProcessed(tick, resource.state)
      become(behaviour, resource)
  }

  private def become(behaviour: () => Behaviour, resource: ResourceData): Unit =
    context.become(base.orElse(behaviour()))
}

object BaseResource {
  private[behaviour] case class Become(behaviour: () => Receive, tick: Int, resource: ResourceData)
}
