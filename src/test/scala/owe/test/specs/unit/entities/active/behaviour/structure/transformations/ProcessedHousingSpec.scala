package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure.{HousingModifier, HousingState}
import owe.entities.active._
import owe.entities.active.behaviour.structure.transformations.ProcessedHousing
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ProcessedHousingSpec extends UnitSpec {
  case class FixtureParam(
    transformer: ProcessedHousing,
    structure: StructureData
  )

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new ProcessedHousing {}

    val structure = StructureData(
      Fixtures.Structure.Housing.properties,
      Fixtures.Structure.Housing.state,
      Fixtures.Structure.Housing.modifiers
    )

    withFixture(test.toNoArgTest(FixtureParam(transformer, structure)))
  }

  "A ProcessedHousing transformation" should "handle entry level updates and commodity shortages" in { fixture =>
    fixture.transformer.withProcessedHousing(
      fixture.structure
    ) should be(
      fixture.structure.state.copy(
        housing = HousingState(
          occupants = 0,
          commodityShortage = 1,
          education = Map(
            EducationEntry("Entry#1") -> EducationLevel(current = 9, minimal = 0, required = 100)
          ),
          entertainment = Map(
            EntertainmentEntry("Entry#2") -> EntertainmentLevel(current = 19, minimal = 0, required = 100)
          ),
          religion = Map(
            ReligionEntry("Entry#3") -> ReligionLevel(current = 29, minimal = 0, required = 100)
          ),
          healthcare = Map(
            HealthcareEntry("Entry#4") -> HealthcareLevel(current = 39, minimal = 0, required = 100)
          ),
          civilService = Map(
            CivilServiceEntry("Entry#5") -> CivilServiceLevel(current = 49, minimal = 0, required = 100)
          )
        )
      )
    )

    {
      val structureWithNoHousingModifiers = fixture.structure.copy(
        state = fixture.structure.state.copy(
          housing = HousingState(
            occupants = 0,
            commodityShortage = 0,
            education = Map(
              EducationEntry("Entry#1") -> EducationLevel(current = 1, minimal = 0, required = 100)
            ),
            entertainment = Map(
              EntertainmentEntry("Entry#2") -> EntertainmentLevel(current = 2, minimal = 0, required = 100)
            ),
            religion = Map(
              ReligionEntry("Entry#3") -> ReligionLevel(current = 3, minimal = 0, required = 100)
            ),
            healthcare = Map(
              HealthcareEntry("Entry#4") -> HealthcareLevel(current = 4, minimal = 0, required = 100)
            ),
            civilService = Map(
              CivilServiceEntry("Entry#5") -> CivilServiceLevel(current = 5, minimal = 0, required = 100)
            )
          )
        ),
        modifiers = fixture.structure.modifiers.copy(
          housing = HousingModifier(
            education = Map.empty,
            entertainment = Map.empty,
            religion = Map.empty,
            healthcare = Map.empty,
            civilService = Map.empty
          )
        )
      )

      fixture.transformer.withProcessedHousing(
        structureWithNoHousingModifiers
      ) should be(
        structureWithNoHousingModifiers.state.copy(
          housing = HousingState(
            occupants = 0,
            commodityShortage = 1,
            education = Map(
              EducationEntry("Entry#1") -> EducationLevel(current = 0, minimal = 0, required = 100)
            ),
            entertainment = Map(
              EntertainmentEntry("Entry#2") -> EntertainmentLevel(current = 1, minimal = 0, required = 100)
            ),
            religion = Map(
              ReligionEntry("Entry#3") -> ReligionLevel(current = 2, minimal = 0, required = 100)
            ),
            healthcare = Map(
              HealthcareEntry("Entry#4") -> HealthcareLevel(current = 3, minimal = 0, required = 100)
            ),
            civilService = Map(
              CivilServiceEntry("Entry#5") -> CivilServiceLevel(current = 4, minimal = 0, required = 100)
            )
          )
        )
      )
    }

    {
      val structureWithNoHousingModifiersAndMinimalEntryLevels = fixture.structure.copy(
        modifiers = fixture.structure.modifiers.copy(
          housing = HousingModifier(
            education = Map.empty,
            entertainment = Map.empty,
            religion = Map.empty,
            healthcare = Map.empty,
            civilService = Map.empty
          )
        )
      )

      fixture.transformer.withProcessedHousing(
        structureWithNoHousingModifiersAndMinimalEntryLevels
      ) should be(
        structureWithNoHousingModifiersAndMinimalEntryLevels.state.copy(
          housing = structureWithNoHousingModifiersAndMinimalEntryLevels.state.housing
            .asInstanceOf[HousingState]
            .copy(
              commodityShortage = 1
            )
        )
      )
    }
  }
}
