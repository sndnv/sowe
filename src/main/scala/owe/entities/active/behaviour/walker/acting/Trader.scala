package owe.entities.active.behaviour.walker.acting

import owe.entities.ActiveEntity.{StructureData, WalkerData}
import owe.entities.active.Walker.CommoditiesState
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.entities.active.behaviour.walker.DistributionCalculations.DistributionResult
import owe.entities.active.behaviour.walker.{BaseWalker, DistributionCalculations}
import owe.entities.active.{Structure, Walker}
import scala.concurrent.{ExecutionContext, Future}

import owe.entities.active.Structure.StructureActorRef

trait Trader extends BaseWalker {
  protected def target: StructureActorRef

  private def transfer(walker: WalkerData)(implicit ec: ExecutionContext): Future[Walker.State] =
    getEntityData(target).map {
      case structure: StructureData =>
        //TODO - check if trade for commodity allowed
        DistributionCalculations.walkerToStructureTransfer(structure, walker) match {
          case Some(DistributionResult(structureCommodities, walkerCommodities)) =>
            distributeCommodities(structure.id, structureCommodities.toSeq)
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

  final override protected def behaviour(implicit ec: ExecutionContext): Behaviour = acting(
    Seq[Action](
      GoToEntity(target),
      DoOperation(transfer),
      GoHome()
    )
  )
}
