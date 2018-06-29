package owe.entities.active.behaviour.resource

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.{EntityBehaviourMessage, ResourceData, UpdateState}
import owe.entities.active.behaviour.BaseBehaviour
import owe.entities.active.behaviour.resource.BaseResource.Become

trait BaseResource extends BaseBehaviour {

  override protected def base: Behaviour = Behaviors.receive { (_, msg) =>
    msg match {
      case Become(behaviour, resource) =>
        parentEntity ! UpdateState(resource.state)
        behaviour()
    }
  }
}

object BaseResource {
  private[behaviour] case class Become(behaviour: () => Behavior[EntityBehaviourMessage], data: ResourceData)
      extends ActiveEntity.Become
}
