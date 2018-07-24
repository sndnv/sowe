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

  import context.dispatcher

  private def transfer(walker: WalkerData): Future[Walker.State] =
    getEntityData(target).map {
      case structure: StructureData =>
        //TODO - check if trade for commodity allowed
        DistributionCalculations.walkerToStructureTransfer(structure, walker) match {
          case Some(DistributionResult(structureCommodities, walkerCommodities)) =>
            distributeCommodities(structure.id, structureCommodities.toSeq)
            walker.state.commodities match {
              case CommoditiesState(available, limits) =>
                walker.state.copy(
                  commodities = CommoditiesState(
                    available = available.mergeWithLimits(walkerCommodities, limits),
                    limits
                  )
                )

              case _ => walker.state //data missing
            }

          case None => walker.state //cannot transfer commodities
        }

      case _ => walker.state //cannot transfer commodities
    }

  final override protected def behaviour: Behaviour = acting(
    Seq[Action](
      GoToEntity(target),
      DoOperation(transfer),
      GoHome()
    )
  )
}
