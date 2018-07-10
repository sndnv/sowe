package owe.entities.active.behaviour.structure

import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure._
import owe.production.Commodity

object CommodityCalculations {
  def production(structure: StructureData): Option[Map[Commodity, Commodity.Amount]] =
    (structure.state.commodities,
     structure.modifiers.commodities,
     structure.state.production,
     structure.modifiers.production) match {
      case (CommoditiesState(available, limits),
            CommoditiesModifier(usageRates),
            ProductionState(employees, _, productionRates),
            ProductionModifier(productionModifierRates)) =>
        if (employees > 0) {
          val maxEmployees = (structure.state.currentStage, structure.properties.stages) match {
            case (DefaultStage, SingleStage(stage))                               => stage.maxPeople
            case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage => stages(stage).maxPeople
            case _                                                                => 0 //stage data missing
          }

          val employeeModifier: Commodity.AmountModifier = Commodity.AmountModifier(employees / maxEmployees * 100)

          val hasEnoughCommodities = usageRates.forall {
            case (commodity, amount) =>
              available.get(commodity).exists(_ >= employeeModifier(amount))
          }

          if (hasEnoughCommodities) {
            val actualProductionRates = productionRates.map {
              case (commodity, amount) =>
                val productionModifier = productionModifierRates.getOrElse(commodity, Commodity.AmountModifier(100))
                val amountModifier = (productionModifier + employeeModifier) / 2

                (commodity, amountModifier(amount))
            }

            val isWithinLimits = actualProductionRates.forall {
              case (commodity, producedAmount) =>
                val currentAmount = available.getOrElse(commodity, Commodity.Amount(0))
                limits.get(commodity).exists(_ >= currentAmount + producedAmount)
            }

            if (isWithinLimits) {
              Some(actualProductionRates)
            } else {
              None //limits reached
            }
          } else {
            None //not enough commodities
          }
        } else {
          None //no employees
        }

      case _ => None //data missing
    }

  def consumption(structure: StructureData): Option[Map[Commodity, Commodity.Amount]] =
    (structure.state.commodities, structure.modifiers.commodities) match {
      case (CommoditiesState(available, _), CommoditiesModifier(usageRates)) =>
        val people = (structure.state.housing, structure.state.production) match {
          case (housing: HousingState, NoProduction)                                => housing.occupants
          case (NoHousing, production: ProductionState) if production.employees > 0 => 1
          case _                                                                    => 0
        }

        if (people > 0) {
          val consumedCommodities = usageRates.collect {
            case (commodity, amount) if available.get(commodity).exists(_ >= (amount * people)) =>
              (commodity, amount)
          }

          if (consumedCommodities.nonEmpty && consumedCommodities.size == usageRates.size) {
            Some(consumedCommodities)
          } else {
            None //not enough commodities
          }
        } else {
          None //no employees or occupants or consumption data is missing
        }

      case _ => None //data missing
    }

  def requiredCommodities(structure: StructureData): Option[Map[Commodity, Commodity.Amount]] =
    (structure.state.commodities, structure.modifiers.commodities) match {
      case (CommoditiesState(available, limits), CommoditiesModifier(usageRates)) =>
        val requiredCommodities = usageRates
          .map {
            case (commodity, _) =>
              val commodityAvailable = available.getOrElse(commodity, Commodity.Amount(0))
              val commodityLimit = limits.getOrElse(commodity, Commodity.Amount(0))

              (commodity, commodityLimit - commodityAvailable)
          }
          .filter(_._2 > Commodity.Amount(0))

        if (requiredCommodities.nonEmpty)
          Some(requiredCommodities)
        else
          None //no required commodities

      case _ => None //data missing
    }

  def areHousingCommoditiesMissing(structure: StructureData): Boolean =
    structure.state.commodities match {
      case CommoditiesState(available, _) =>
        CommodityCalculations.requiredCommodities(structure) match {
          case Some(requiredCommodities) =>
            requiredCommodities.exists {
              case (commodity, required) =>
                !available.get(commodity).exists(_ >= required)
            }

          case None => false //data missing
        }

      case _ => false //data missing
    }
}
