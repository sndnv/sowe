package owe.entities.active.behaviour.structure.transformations

import owe.entities.ActiveEntity.StructureData
import owe.entities.Entity
import owe.entities.Entity._
import owe.entities.active.Structure.{State, _}
import owe.production.Commodity

trait ProcessedUpdateMessages {
  def withProcessedUpdateMessages(structure: StructureData, pendingMessages: Seq[Entity.Message]): State =
    pendingMessages.foldLeft(structure.state) {
      case (currentState, message) =>
        message match {
          case ProcessCommodities(commodities) =>
            currentState.commodities match {
              case CommoditiesState(available, limits) =>
                val occupants = currentState.housing match {
                  case housing: HousingState => housing.occupants
                  case NoHousing             => 0
                }

                val updatedCommodities = available ++ commodities.map {
                  case (commodity, amount) =>
                    val limitAmount = limits.getOrElse(commodity, Commodity.Amount(0))
                    val updatedCommodityAmount =
                      (available.getOrElse(commodity, Commodity.Amount(0)) + amount)
                        .min(if (occupants > 0) limitAmount * occupants else limitAmount)
                        .max(Commodity.Amount(0))

                    (commodity, updatedCommodityAmount)
                }
                currentState.copy(commodities = CommoditiesState(updatedCommodities, limits))

              case NoCommodities => currentState //can't update commodities; data missing
            }

          case ProcessOccupantsUpdate(occupants) =>
            currentState.housing match {
              case housing: HousingState =>
                val maxPeople = (currentState.currentStage, structure.properties.stages) match {
                  case (DefaultStage, SingleStage(stage))                               => stage.maxPeople
                  case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage => stages(stage).maxPeople
                  case _                                                                => 0 //stage data missing
                }

                currentState.copy(
                  housing = housing.copy(
                    occupants = (housing.occupants + occupants).min(maxPeople).max(0)
                  )
                )

              case NoHousing => currentState //can't update housing; data missing
            }

          case ProcessLabourUpdate(employees) =>
            currentState.production match {
              case production: ProductionState =>
                val maxPeople = (currentState.currentStage, structure.properties.stages) match {
                  case (DefaultStage, SingleStage(stage))                               => stage.maxPeople
                  case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage => stages(stage).maxPeople
                  case _                                                                => 0 //stage data missing
                }

                currentState.copy(
                  production = production.copy(
                    employees = (production.employees + employees).min(maxPeople).max(0)
                  )
                )

              case NoProduction => currentState //can't update labour; data missing
            }

          case ProcessAttack(damage) =>
            currentState.copy(currentLife = damage(currentState.currentLife))

          case ProcessLabourFound() =>
            currentState.production match {
              case production: ProductionState =>
                production.labour match {
                  case LabourState.Looking | LabourState.None =>
                    currentState.copy(production = production.copy(labour = LabourState.Found))

                  case LabourState.Found => currentState //do nothing; labour already found
                }

              case _ => currentState //can't update labour; not necessary or data missing
            }

          case _ => currentState
        }
    }
}
