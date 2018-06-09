package owe.entities.active.behaviour.structure.transformations

import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure.{HousingModifier, HousingState, State}
import owe.entities.active._
import owe.entities.active.behaviour.structure.CommodityCalculations

trait ProcessedHousing {
  def withProcessedHousing(structure: StructureData): State =
    (structure.state.housing, structure.modifiers.housing) match {
      case (housing @ HousingState(_, commodityShortage, education, entertainment, religion, healthcare, civilService),
            HousingModifier(educationModifier,
                            entertainmentModifier,
                            religionModifier,
                            healthcareModifier,
                            civilServiceModifier)) =>
        val updatedEducation = education.map {
          case (entry, level) =>
            val actualModifier = educationModifier.getOrElse(entry, EducationLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }

        val updatedEntertainment = entertainment.map {
          case (entry, level) =>
            val actualModifier = entertainmentModifier.getOrElse(entry, EntertainmentLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }

        val updatedReligion = religion.map {
          case (entry, level) =>
            val actualModifier = religionModifier.getOrElse(entry, ReligionLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }

        val updatedHealthcare = healthcare.map {
          case (entry, level) =>
            val actualModifier = healthcareModifier.getOrElse(entry, HealthcareLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }

        val updatedCivilService = civilService.map {
          case (entry, level) =>
            val actualModifier = civilServiceModifier.getOrElse(entry, CivilServiceLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }

        structure.state.copy(
          housing = housing.copy(
            commodityShortage =
              if (CommodityCalculations.areHousingCommoditiesMissing(structure))
                commodityShortage + 1
              else
                0,
            education = updatedEducation,
            entertainment = updatedEntertainment,
            religion = updatedReligion,
            healthcare = updatedHealthcare,
            civilService = updatedCivilService
          )
        )

      case _ => structure.state //can't update housing; data missing
    }
}
