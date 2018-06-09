package owe.entities.active.behaviour.structure

import akka.actor.Actor.Receive
import owe.entities.ActiveEntity.StructureData
import owe.entities.Entity.{State => _}
import owe.entities.active.Structure._
import owe.entities.active._
import owe.entities.active.behaviour.structure.BaseStructure.Become
import owe.entities.active.behaviour.{BaseBehaviour, UpdateExchange}
import owe.production.{CommodityAmount, CommodityState}

trait BaseStructure extends BaseBehaviour[Structure.ActorRefTag] {
  private def base(): Behaviour = {
    case Become(behaviour, structure) =>
      parentEntity ! structure.state
      become(behaviour, structure)
  }

  private def become(behaviour: () => Behaviour, structure: StructureData): Unit =
    if (structure.state.currentLife.isSufficient) {
      context.become(base().orElse(behaviour()))
    } else {
      structure.state.commodities match {
        case CommoditiesState(available, _) =>
          UpdateExchange.State(available.filter(_._2 > CommodityAmount(0)), CommodityState.Lost)

        case _ => //do nothing
      }
    }
}

object BaseStructure {
  private[behaviour] case class Become(behaviour: () => Receive, structure: StructureData)

  private[behaviour] sealed trait StructureTransition
  private[behaviour] object StructureTransition {
    case object Upgrade extends StructureTransition
    case object Downgrade extends StructureTransition
    case object None extends StructureTransition
  }
}