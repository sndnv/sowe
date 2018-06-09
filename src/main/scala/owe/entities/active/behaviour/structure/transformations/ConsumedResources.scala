package owe.entities.active.behaviour.structure.transformations

import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure.{CommoditiesModifier, CommoditiesState, State}
import owe.entities.active.behaviour.structure.CommodityCalculations
import owe.production.CommodityAmount

trait ConsumedResources {
  def withConsumedResources(structure: StructureData): State =
    (structure.state.commodities, structure.modifiers.commodities) match {
      case (CommoditiesState(available, limits), _: CommoditiesModifier) =>
        CommodityCalculations.consumption(structure) match {
          case Some(consumedCommodities) =>
            val updatedResources = available ++ consumedCommodities.map {
              case (commodity, amount) =>
                (commodity, available.getOrElse(commodity, CommodityAmount(0)) - amount)
            }

            structure.state.copy(
              commodities = CommoditiesState(updatedResources, limits)
            )

          case None => structure.state //can't update commodities; data missing
        }

      case _ => structure.state //can't update commodities; data missing
    }
}
