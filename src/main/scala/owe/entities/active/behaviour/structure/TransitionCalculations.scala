package owe.entities.active.behaviour.structure

import owe.CellDesirability
import owe.entities.ActiveEntity.MapData
import owe.entities.active.Structure._
import owe.entities.active.behaviour.structure.BaseStructure.StructureTransition

object TransitionCalculations {
  def housing(
    map: MapData,
    housingState: HousingState,
    currentStage: Stages with StateOnly,
    definedStages: Stages with PropertiesOnly,
    areHousingCommoditiesMissing: Boolean
  ): StructureTransition = {
    val HousingState(
      occupants,
      commodityShortage,
      education,
      entertainment,
      religion,
      healthcare,
      civilService
    ) = housingState

    if (occupants > 0) {
      val cellDesirability = map.cellModifiers
        .desirability(map.cellProperties.desirability)
        .min(CellDesirability.Max)

      (currentStage, definedStages) match {
        case (DefaultStage, SingleStage(_)) =>
          StructureTransition.None //can't transition from single stage

        case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage =>
          val enoughDesirability = cellDesirability >= stages(stage).minDesirability

          val shouldDowngrade =
            (areHousingCommoditiesMissing && commodityShortage >= stages(stage).commodityShortageLimit) ||
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
              !areHousingCommoditiesMissing &&
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

        case _ =>
          StructureTransition.None //stage data incorrect or missing
      }
    } else {
      StructureTransition.None //no transition; no occupants
    }
  }

  def producing(
    map: MapData,
    currentStage: Stages with StateOnly,
    definedStages: Stages with PropertiesOnly
  ): StructureTransition = {
    val cellDesirability = map.cellModifiers
      .desirability(map.cellProperties.desirability)
      .min(CellDesirability.Max)

    (currentStage, definedStages) match {
      case (DefaultStage, SingleStage(_)) =>
        StructureTransition.None //can't transition from single stage

      case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage =>
        if (cellDesirability > stages(stage).minDesirability) {
          StructureTransition.Upgrade
        } else if (cellDesirability < stages(stage).minDesirability) {
          StructureTransition.Downgrade
        } else {
          StructureTransition.None
        }

      case _ =>
        StructureTransition.None //stage data incorrect or missing
    }
  }
}
