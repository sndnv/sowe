package owe.entities.active.behaviour.walker.acting

import owe.entities.ActiveEntity.{StructureData, WalkerData}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker
import owe.entities.active.Walker.CommoditiesState
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.entities.active.behaviour.walker.DistributionCalculations.DistributionResult
import owe.entities.active.behaviour.walker.{BaseWalker, DistributionCalculations}
import owe.production.Commodity

import scala.concurrent.Future

trait Trader extends BaseWalker {
  protected def target: StructureRef

  // TODO - check if trade for commodities is allowed
  protected def selling: Seq[Commodity]
  protected def buying: Seq[Commodity]

  import context.dispatcher

  private def sell(walker: WalkerData): Future[Walker.State] =
    getEntityData(target).map {
      case structure: StructureData =>
        DistributionCalculations.walkerToStructureTransfer(structure, walker) match {
          case Some(DistributionResult(walkerCommodities, structureCommodities)) =>
            distributeCommodities(structure.id, structureCommodities.filterKeys(selling.contains).toSeq)
            walker.state.commodities match {
              case CommoditiesState(available, limits) =>
                walker.state.copy(
                  commodities = CommoditiesState(
                    available = available.mergeWithLimits(walkerCommodities.filterKeys(selling.contains), limits),
                    limits
                  )
                )

              case _ => walker.state // data missing
            }

          case None => walker.state // cannot transfer commodities
        }

      case _ => walker.state // cannot transfer commodities
    }

  private def buy(walker: WalkerData): Future[Walker.State] =
    getEntityData(target).map {
      case structure: StructureData =>
        DistributionCalculations.structureToWalkerTransfer(structure, walker) match {
          case Some(DistributionResult(structureCommodities, walkerCommodities)) =>
            distributeCommodities(structure.id, structureCommodities.filterKeys(buying.contains).toSeq)
            walker.state.commodities match {
              case CommoditiesState(available, limits) =>
                walker.state.copy(
                  commodities = CommoditiesState(
                    available = available.mergeWithLimits(walkerCommodities.filterKeys(buying.contains), limits),
                    limits
                  )
                )

              case _ => walker.state // data missing
            }

          case None => walker.state // cannot transfer commodities
        }

      case _ => walker.state // cannot transfer commodities
    }

  final override protected def behaviour: Behaviour = acting(
    Seq[Action](
      GoToEntity(target),
      DoOperation(sell),
      DoOperation(buy),
      GoHome()
    )
  )
}
