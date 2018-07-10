package owe.entities.active.behaviour.structure.transformations

import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure.{CommoditiesState, State}
import owe.entities.active.behaviour.structure.CommodityCalculations
import owe.production.Commodity

trait ProducedResources {
  def withProducedResources(structure: StructureData): State =
    structure.state.commodities match {
      case CommoditiesState(available, limits) =>
        CommodityCalculations.production(structure) match {
          case Some(actualProductionRates) =>
            val updatedResources = available ++ actualProductionRates.map {
              case (commodity, amount) =>
                (commodity, available.getOrElse(commodity, Commodity.Amount(0)) + amount)
            }

            structure.state.copy(
              commodities = CommoditiesState(updatedResources, limits)
            )

          case None => structure.state
        }

      case _ => structure.state
    }
}
