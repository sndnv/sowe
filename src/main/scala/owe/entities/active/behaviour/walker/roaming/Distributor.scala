package owe.entities.active.behaviour.walker.roaming

import owe.entities.ActiveEntity.{StructureData, WalkerData}
import owe.entities.active.Walker
import owe.entities.active.Walker.CommoditiesState
import owe.entities.active.attributes.Distance
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.entities.active.behaviour.walker.DistributionCalculations.DistributionResult
import owe.entities.active.behaviour.walker.{BaseWalker, DistributionCalculations}
import owe.production.Commodity

import scala.concurrent.Future

trait Distributor extends BaseWalker {
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
                currentState.commodities match {
                  case CommoditiesState(available, limits) =>
                    currentState.copy(
                      commodities = CommoditiesState(
                        available = available.mergeWithLimits(walkerCommodities, limits),
                        limits
                      )
                    )

                  case _ => walker.state //data missing
                }

              case None => currentState //cannot transfer commodities
            }
        }
      }

  private def continueRoaming(walker: WalkerData): Boolean =
    walker.state.commodities match {
      case CommoditiesState(available, _) => available.exists(_._2 > Commodity.Amount(0))
      case _                              => false //no commodities to work with
    }
}
