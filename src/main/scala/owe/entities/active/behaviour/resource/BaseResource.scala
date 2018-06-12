package owe.entities.active.behaviour.resource

import akka.actor.Actor.Receive
import owe.entities.ActiveEntity.ResourceData
import owe.entities.active.Resource
import owe.entities.active.behaviour.BaseBehaviour
import owe.entities.active.behaviour.resource.BaseResource.Become

trait BaseResource extends BaseBehaviour[Resource.ActorRefTag] {

  override protected def base: Behaviour = {
    case Become(behaviour, resource) =>
      parentEntity ! resource.state
      become(behaviour, resource)
  }

  private def become(behaviour: () => Behaviour, resource: ResourceData): Unit =
    context.become(base.orElse(behaviour()))
}

object BaseResource {
  private[behaviour] case class Become(behaviour: () => Receive, resource: ResourceData)
}
