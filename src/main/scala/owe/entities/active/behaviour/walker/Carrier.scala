package owe.entities.active.behaviour.walker

import owe.EntityID
import owe.entities.ActiveEntity.{StructureData, WalkerData}
import owe.entities.active.Walker
import owe.entities.active.Walker.{CommoditiesState, MovementMode}
import owe.entities.active.behaviour.walker.BaseDistributor.DistributionResult
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.production.CommodityAmount
import owe.production.Exchange.CommodityInTransit

import scala.concurrent.Future

trait Carrier extends BaseWalker with BaseDistributor {
  protected def target: EntityID
  protected def source: EntityID
  protected def actions: Seq[Action]
  protected def canReturnCommodities: Boolean

  import context.dispatcher

  private def load(walker: WalkerData, target: EntityID): Future[Walker.State] =
    getEntityData(target).map {
      case structure: StructureData =>
        calculateStructureToWalkerTransfer(structure, walker) match {
          case Some(DistributionResult(structureCommodities, walkerCommodities)) =>
            distributeCommodities(structure.properties.id, structureCommodities.toSeq)

            val messages = walkerCommodities.map {
              case (commodity, amount) if amount > CommodityAmount(0) =>
                CommodityInTransit(commodity, amount, walker.properties.id, target)
            }

            forwardExchangeMessages(messages.toSeq)

            walker.state.commodities match {
              case CommoditiesState(available, limits) =>
                walker.state.copy(
                  commodities = CommoditiesState(available = available ++ walkerCommodities, limits)
                )

              case _ => walker.state //data missing
            }

          case None => walker.state //cannot transfer commodities
        }

      case _ => walker.state //cannot transfer commodities
    }

  private def unload(walker: WalkerData, target: EntityID): Future[Walker.State] =
    getEntityData(target).map {
      case structure: StructureData =>
        calculateWalkerToStructureTransfer(structure, walker) match {
          case Some(DistributionResult(structureCommodities, walkerCommodities)) =>
            distributeCommodities(structure.properties.id, structureCommodities.toSeq)

            val messages = walkerCommodities.map {
              case (commodity, amount) if amount > CommodityAmount(0) =>
                CommodityInTransit(commodity, amount, walker.properties.id, target)
            }

            forwardExchangeMessages(messages.toSeq)

            walker.state.commodities match {
              case CommoditiesState(available, limits) =>
                walker.state.copy(
                  commodities = CommoditiesState(available = available ++ walkerCommodities, limits)
                )

              case _ => walker.state //data missing
            }

          case None => walker.state //cannot transfer commodities
        }

      case _ => walker.state //cannot transfer commodities
    }

  private def unloadComplete(walker: WalkerData): Boolean =
    (walker.state.mode, canReturnCommodities) match {
      case (MovementMode.Returning, true)  => false //wait for enough free space
      case (MovementMode.Returning, false) => true //can't wait for free space; done
      case (_, true)                       => true //return with remaining commodities
      case (_, false)                      => false //wait for enough free space
    }

  final protected def retrieveResources: Seq[Action] = Seq[Action](
    GoToEntity(target),
    DoOperation(load(_, target)),
    GoHome(),
    DoRepeatableOperation(unload(_, source), unloadComplete)
  )

  final protected def deliver: Seq[Action] = Seq[Action](
    DoOperation(load(_, source)),
    GoToEntity(target),
    DoRepeatableOperation(unload(_, target), unloadComplete),
    GoHome(),
    DoRepeatableOperation(unload(_, source), unloadComplete)
  )

  final override protected def behaviour: Behaviour = acting(actions)
}
