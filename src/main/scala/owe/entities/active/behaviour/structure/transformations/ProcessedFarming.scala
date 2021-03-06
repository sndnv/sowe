package owe.entities.active.behaviour.structure.transformations

import owe.entities.ActiveEntity.{MapData, StructureData}
import owe.entities.active.Structure.{CommoditiesState, State}
import owe.entities.active.behaviour.structure.CommodityCalculations
import owe.map.Cell.{Fertility, Water}
import owe.production.Commodity

trait ProcessedFarming {
  def withProcessedFarming(map: MapData, structure: StructureData): State =
    structure.state.commodities match {
      case CommoditiesState(available, limits) =>
        CommodityCalculations.production(structure) match {
          case Some(actualProductionRates) =>
            val updatedResources = available ++ actualProductionRates.map {
              case (commodity, amount) =>
                val water = map.cellState.water
                  .min(Water.Max)
                  .max(Water.Min)

                val fertility = map.cellState.fertility
                  .min(Fertility.Max)
                  .max(Fertility.Min)

                val actualAmount = amount.basedOn(fertility.basedOn(water))

                (commodity, available.getOrElse(commodity, Commodity.Amount(0)) + actualAmount)
            }

            structure.state.copy(
              commodities = CommoditiesState(updatedResources, limits)
            )

          case None => structure.state
        }

      case _ => structure.state
    }
}
