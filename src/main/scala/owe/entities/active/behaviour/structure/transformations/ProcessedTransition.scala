package owe.entities.active.behaviour.structure.transformations

import owe.CellDesirability
import owe.entities.ActiveEntity.{MapData, StructureData}
import owe.entities.active.Structure._
import owe.entities.active.behaviour.structure.BaseStructure.StructureTransition
import owe.entities.active.behaviour.structure.CommodityCalculations

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
      case HousingState(occupants, commodityShortage, education, entertainment, religion, healthcare, civilService) =>
        if (occupants > 0) {
          val commoditiesMissing = CommodityCalculations.areHousingCommoditiesMissing(structure)
          val cellDesirability =
            map.cellModifiers.desirability(map.cellProperties.desirability).min(CellDesirability.Max)

          val (minDesirability, commodityShortageLimit) =
            (structure.state.currentStage, structure.properties.stages) match {
              case (DefaultStage, SingleStage(stage)) =>
                (stage.minDesirability, stage.commodityShortageLimit)

              case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage =>
                (stages(stage).minDesirability, stages(stage).commodityShortageLimit)

              case _ =>
                (CellDesirability.Neutral, 0) //stage data missing
            }

          val enoughDesirability = cellDesirability >= minDesirability

          val shouldDowngrade =
            (commoditiesMissing && commodityShortage >= commodityShortageLimit) ||
              !enoughDesirability ||
              education.forall { case (_, level)     => level.current < level.minimal } ||
              entertainment.forall { case (_, level) => level.current < level.minimal } ||
              religion.forall { case (_, level)      => level.current < level.minimal } ||
              healthcare.forall { case (_, level)    => level.current < level.minimal } ||
              civilService.forall { case (_, level)  => level.current < level.minimal }

          if (shouldDowngrade) {
            StructureTransition.Downgrade
          } else {
            val shouldUpgrade =
              !commoditiesMissing &&
                enoughDesirability &&
                education.forall { case (_, level)     => level.current >= level.required } &&
                entertainment.forall { case (_, level) => level.current >= level.required } &&
                religion.forall { case (_, level)      => level.current >= level.required } &&
                healthcare.forall { case (_, level)    => level.current >= level.required } &&
                civilService.forall { case (_, level)  => level.current >= level.required }

            if (shouldUpgrade) {
              StructureTransition.Upgrade
            } else {
              StructureTransition.None //no transition; no need to upgrade or downgrade
            }
          }

        } else {
          StructureTransition.None //no transition; no occupants
        }

      case _ =>
        val cellDesirability =
          map.cellModifiers.desirability(map.cellProperties.desirability).min(CellDesirability.Max)

        val minDesirability =
          (structure.state.currentStage, structure.properties.stages) match {
            case (DefaultStage, SingleStage(stage)) =>
              stage.minDesirability

            case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage =>
              stages(stage).minDesirability

            case _ =>
              CellDesirability.Neutral //stage data missing
          }

        if (cellDesirability > minDesirability) {
          StructureTransition.Upgrade
        } else if (cellDesirability < minDesirability) {
          StructureTransition.Downgrade
        } else {
          StructureTransition.None
        }
    }
}
