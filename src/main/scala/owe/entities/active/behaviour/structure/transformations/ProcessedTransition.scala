package owe.entities.active.behaviour.structure.transformations

import owe.entities.ActiveEntity.{MapData, StructureData}
import owe.entities.active.Structure._
import owe.entities.active.behaviour.structure.BaseStructure.StructureTransition
import owe.entities.active.behaviour.structure.{CommodityCalculations, TransitionCalculations}

trait ProcessedTransition {
  def withProcessedTransition(map: MapData, structure: StructureData): State =
    (structure.state.currentStage, structure.properties.stages) match {
      case (CurrentStage(stage), MultiStage(stages)) =>
        calculateStructureTransition(map, structure) match {
          case StructureTransition.Upgrade =>
            val nextStage = stage + 1
            if (nextStage >= stages.size) {
              structure.state
            } else {
              structure.state.copy(currentStage = CurrentStage(nextStage))
            }

          case StructureTransition.Downgrade =>
            val nextStage = stage - 1
            if (nextStage >= stages.size || nextStage < 0) {
              structure.state
            } else {
              structure.state.copy(currentStage = CurrentStage(nextStage))
            }

          case StructureTransition.None =>
            structure.state //no transition needed
        }

      case _ =>
        structure.state //no transition needed
    }

  private def calculateStructureTransition(map: MapData, structure: StructureData): StructureTransition =
    structure.state.housing match {
      case housingState: HousingState =>
        TransitionCalculations.housing(
          map,
          housingState,
          structure.state.currentStage,
          structure.properties.stages,
          CommodityCalculations.areHousingCommoditiesMissing(structure)
        )

      case _ =>
        TransitionCalculations.producing(
          map,
          structure.state.currentStage,
          structure.properties.stages
        )
    }
}
