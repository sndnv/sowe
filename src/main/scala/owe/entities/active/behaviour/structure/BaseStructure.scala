package owe.entities.active.behaviour.structure

import akka.actor.Actor.Receive
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor._
import owe.entities.Entity.{State => _}
import owe.entities.active.Structure._
import owe.entities.active.behaviour.structure.BaseStructure.Become
import owe.entities.active.behaviour.{BaseBehaviour, UpdateExchange}
import owe.map.GameMap.DestroyEntity
import owe.production.Commodity

trait BaseStructure extends BaseBehaviour {

  final override private[behaviour] implicit val parentEntity: ActiveEntity.ActiveEntityRef =
    StructureRef(context.parent)

  override protected def base: Behaviour = {
    case CreateBehaviour(structure: StructureData) =>
      structure.state.production match {
        case ProductionState(_, _, rates) =>
          rates.foreach {
            case (commodity, _) => UpdateExchange.Producers.add(parentEntity, commodity)
          }

        case _ => //do nothing
      }

      structure.modifiers.commodities match {
        case CommoditiesModifier(usageRates) =>
          usageRates.foreach {
            case (commodity, _) => UpdateExchange.Consumers.add(parentEntity, commodity)
          }

        case _ => //do nothing
      }

    case DestroyBehaviour(structure: StructureData) =>
      structure.state.production match {
        case ProductionState(_, _, rates) =>
          rates.foreach {
            case (commodity, _) => UpdateExchange.Producers.remove(parentEntity, commodity)
          }

        case _ => //do nothing
      }

      structure.modifiers.commodities match {
        case CommoditiesModifier(usageRates) =>
          usageRates.foreach {
            case (commodity, _) => UpdateExchange.Consumers.remove(parentEntity, commodity)
          }

        case _ => //do nothing
      }

      structure.state.commodities match {
        case CommoditiesState(available, _) =>
          UpdateExchange.State(available.filter(_._2 > Commodity.Amount(0)), Commodity.State.Lost)
          UpdateExchange.Stats.availableCommodities(parentEntity, available.mapValues(_ => Commodity.Amount(0)))

        case _ => //do nothing
      }

      structure.modifiers.commodities match {
        case CommoditiesModifier(usageRates) =>
          UpdateExchange.Stats.requiredCommodities(parentEntity, usageRates.mapValues(_ => Commodity.Amount(0)))

        case _ => //do nothing
      }

      parentEntity ! BehaviourDestroyed()
      context.become(destroying())

    case Become(behaviour, structure) =>
      parentEntity ! BehaviourTickProcessed(structure.state)
      become(behaviour, structure)
  }

  protected def destroying(): Behaviour = {
    case ApplyInstructions(_, _) =>
      log.debug("Entity [{}] waiting to be destroyed; instructions application ignored", self)
      parentEntity ! InstructionsApplied()

    case ApplyMessages(structure: StructureData, _) =>
      log.debug("Entity [{}] waiting to be destroyed; messages application ignored", self)
      parentEntity ! MessagesApplied(structure.state)

    case ProcessBehaviourTick(_, entity) =>
      log.debug("Entity [{}] waiting to be destroyed; tick ignored", self)
      parentEntity ! BehaviourTickProcessed(entity.state)
  }

  private def become(behaviour: () => Behaviour, structure: StructureData): Unit = {
    if (!structure.state.currentLife.isSufficient) {
      parentEntity ! ForwardMessage(DestroyEntity(structure.id))
    }

    context.become(base.orElse(behaviour()))
  }
}

object BaseStructure {
  private[behaviour] case class Become(behaviour: () => Receive, structure: StructureData)

  sealed trait StructureTransition
  object StructureTransition {
    case object Upgrade extends StructureTransition
    case object Downgrade extends StructureTransition
    case object None extends StructureTransition
  }
}
