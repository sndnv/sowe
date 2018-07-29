package owe.entities.active.behaviour.walker.acting

import owe.entities.ActiveEntity.{ResourceData, StructureData, WalkerData}
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker
import owe.entities.active.Walker.CommoditiesState
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.walker.DistributionCalculations.DistributionResult
import owe.entities.active.behaviour.walker.{BaseWalker, DistributionCalculations}

import scala.concurrent.Future

trait ActingWalker extends BaseWalker {

  import context.dispatcher

  protected def gather(walker: WalkerData, source: ResourceRef, target: StructureRef): Future[Walker.State] =
    getEntityData(source).map {
      case resource: ResourceData =>
        DistributionCalculations.resourceToWalkerTransfer(resource, walker) match {
          case Some(DistributionResult(resourceCommodities, walkerCommodities)) =>
            distributeCommodities(resource.id, resourceCommodities.toSeq)

            walker.state.commodities match {
              case CommoditiesState(available, limits) =>
                val updatedAvailable = available.mergeWithLimits(walkerCommodities, limits)

                UpdateExchange.Stats.inTransitCommodities(
                  walker.id,
                  target,
                  updatedAvailable
                )

                walker.state.copy(
                  commodities = CommoditiesState(
                    available = updatedAvailable,
                    limits
                  )
                )

              case _ => walker.state //data missing
            }

          case None => walker.state //cannot transfer commodities
        }

      case _ => walker.state //cannot transfer commodities
    }

  protected def load(walker: WalkerData, source: StructureRef, target: StructureRef): Future[Walker.State] =
    getEntityData(source).map {
      case structure: StructureData =>
        DistributionCalculations.structureToWalkerTransfer(structure, walker) match {
          case Some(DistributionResult(structureCommodities, walkerCommodities)) =>
            distributeCommodities(structure.id, structureCommodities.toSeq)

            walker.state.commodities match {
              case CommoditiesState(available, limits) =>
                val updatedAvailable = available.mergeWithLimits(walkerCommodities, limits)

                UpdateExchange.Stats.inTransitCommodities(
                  walker.id,
                  target,
                  updatedAvailable
                )

                walker.state.copy(
                  commodities = CommoditiesState(
                    available = updatedAvailable,
                    limits
                  )
                )

              case _ => walker.state //data missing
            }

          case None => walker.state //cannot transfer commodities
        }

      case _ => walker.state //cannot transfer commodities
    }

  protected def unload(walker: WalkerData, target: StructureRef): Future[Walker.State] =
    getEntityData(target).map {
      case structure: StructureData =>
        DistributionCalculations.walkerToStructureTransfer(structure, walker) match {
          case Some(DistributionResult(walkerCommodities, structureCommodities)) =>
            distributeCommodities(structure.id, structureCommodities.toSeq)

            walker.state.commodities match {
              case CommoditiesState(available, limits) =>
                val updatedAvailable = available.mergeWithLimits(walkerCommodities, limits)

                UpdateExchange.Stats.inTransitCommodities(
                  walker.id,
                  target,
                  updatedAvailable
                )

                walker.state.copy(
                  commodities = CommoditiesState(
                    available = updatedAvailable,
                    limits
                  )
                )

              case _ => walker.state //data missing
            }

          case None => walker.state //cannot transfer commodities
        }

      case _ => walker.state //cannot transfer commodities
    }
}
