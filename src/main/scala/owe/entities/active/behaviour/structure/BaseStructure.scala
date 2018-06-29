package owe.entities.active.behaviour.structure

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.{EntityBehaviourMessage, ProcessEntityTick, StructureData, UpdateState}
import owe.entities.Entity.{State => _}
import owe.entities.active.Structure._
import owe.entities.active._
import owe.entities.active.behaviour.structure.BaseStructure.Become
import owe.entities.active.behaviour.{BaseBehaviour, UpdateExchange}
import owe.production.{CommodityAmount, CommodityState}

trait BaseStructure extends BaseBehaviour {
  override protected def base: Behaviour = Behaviors.receive { (_, msg) =>
    msg match {
      case Become(behaviour, structure) =>
        parentEntity ! UpdateState(structure.state)

        if (structure.state.currentLife.isSufficient) {
          behaviour()
        } else {
          structure.state.commodities match {
            case CommoditiesState(available, _) =>
              UpdateExchange.State(available.filter(_._2 > CommodityAmount(0)), CommodityState.Lost)

            case _ => //do nothing
          }

          destroying()
        }
    }
  }

  protected def destroying(): Behaviour = Behaviors.receive { (ctx, msg) =>
    msg match {
      case ProcessEntityTick(_, entity, _) =>
        ctx.log.debug("Entity [{}] waiting to be destroyed; tick ignored", ctx.self)
        parentEntity ! UpdateState(entity.state)
    }

    Behaviors.stopped // TODO
  }
}

object BaseStructure {
  private[behaviour] case class Become(behaviour: () => Behavior[EntityBehaviourMessage], data: StructureData)
      extends ActiveEntity.Become

  sealed trait StructureTransition
  object StructureTransition {
    case object Upgrade extends StructureTransition
    case object Downgrade extends StructureTransition
    case object None extends StructureTransition
  }
}
