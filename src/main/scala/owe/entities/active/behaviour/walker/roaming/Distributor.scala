package owe.entities.active.behaviour.walker.roaming

import owe.entities.ActiveEntity.{StructureData, WalkerData}
import owe.entities.active.Walker.CommoditiesState
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.entities.active.behaviour.walker.DistributionCalculations.DistributionResult
import owe.entities.active.behaviour.walker.{BaseWalker, DistributionCalculations}
import owe.entities.active.{Distance, Structure, Walker}
import owe.production.CommodityAmount

import scala.concurrent.Future

trait Distributor extends BaseWalker {
  protected def target: Structure.ActiveEntityActorRef
  protected def distributionRadius: Distance

  import context.dispatcher

  final override protected def behaviour: Behaviour = roaming(DoRepeatableOperation(distribute, continueRoaming))

  private def distribute(walker: WalkerData): Future[Walker.State] =
    getNeighboursData(walker.id, distributionRadius)
      .map { entities =>
        entities.foldLeft(walker.state) {
          case (currentState, (_, structure: StructureData)) =>
            DistributionCalculations.walkerToStructureTransfer(
              structure,
              walker.copy(state = currentState)
            ) match {
              case Some(DistributionResult(structureCommodities, walkerCommodities)) =>
                distributeCommodities(structure.id, structureCommodities.toSeq)
                walker.state.commodities match {
                  case CommoditiesState(available, limits) =>
                    walker.state.copy(
                      commodities = CommoditiesState(available = available ++ walkerCommodities, limits)
                    )

                  case _ => walker.state //data missing
                }

              case None => currentState //cannot transfer commodities
            }
        }
      }

  private def continueRoaming(walker: WalkerData): Boolean =
    walker.state.commodities match {
      case CommoditiesState(available, _) => available.exists(_._2 > CommodityAmount(0))
      case _                              => false //no commodities to work with
    }
}
