package owe.test.specs.unit.entities.active.behaviour.structure

import org.scalatest.Outcome
import owe.CellDesirability
import owe.entities.active.Structure._
import owe.entities.active._
import owe.entities.active.behaviour.structure.BaseStructure.StructureTransition
import owe.entities.active.behaviour.structure.TransitionCalculations
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class TransitionCalculationsSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "TransitionCalculations" should "calculate housing structure transitions" in { _ =>
    val map = Fixtures.defaultMapData.copy(
      cellState = Fixtures.defaultCellState.copy(desirability = CellDesirability(4))
    )

    val stages = MultiStage(
      stages = Seq(
        StageProperties(
          maxLife = Life(100),
          maxPeople = 5,
          minDesirability = CellDesirability.Neutral,
          commodityShortageLimit = 10
        ),
        StageProperties(
          maxLife = Life(150),
          maxPeople = 15,
          minDesirability = CellDesirability(4),
          commodityShortageLimit = 5
        ),
        StageProperties(
          maxLife = Life(200),
          maxPeople = 50,
          minDesirability = CellDesirability.Max,
          commodityShortageLimit = 3
        )
      )
    )

    val upgradableHousingState = HousingState(
      occupants = 5,
      commodityShortage = 0,
      education = Map(
        EducationEntry("Entry#1") -> EducationLevel(current = 10, minimal = 0, required = 10)
      ),
      entertainment = Map(
        EntertainmentEntry("Entry#2") -> EntertainmentLevel(current = 10, minimal = 0, required = 10)
      ),
      religion = Map(
        ReligionEntry("Entry#3") -> ReligionLevel(current = 10, minimal = 0, required = 10)
      ),
      healthcare = Map(
        HealthcareEntry("Entry#4") -> HealthcareLevel(current = 10, minimal = 0, required = 10)
      ),
      civilService = Map(
        CivilServiceEntry("Entry#5") -> CivilServiceLevel(current = 10, minimal = 0, required = 10)
      )
    )

    // single stage; no transition
    TransitionCalculations.housing(
      map,
      currentStage = DefaultStage,
      definedStages = SingleStage(
        stage = StageProperties(
          maxLife = Life(100),
          maxPeople = 15,
          minDesirability = CellDesirability.Neutral,
          commodityShortageLimit = 0
        )
      ),
      housingState = upgradableHousingState,
      areHousingCommoditiesMissing = false
    ) should be(StructureTransition.None)

    // multi-stage; upgrade
    TransitionCalculations.housing(
      map,
      currentStage = CurrentStage(0),
      definedStages = stages,
      housingState = upgradableHousingState,
      areHousingCommoditiesMissing = false
    ) should be(StructureTransition.Upgrade)

    // multi-stage; downgrade (not enough desirability)
    TransitionCalculations.housing(
      map,
      currentStage = CurrentStage(2),
      definedStages = stages,
      housingState = upgradableHousingState,
      areHousingCommoditiesMissing = false
    ) should be(StructureTransition.Downgrade)

    // multi-stage; downgrade (commodity shortage)
    TransitionCalculations.housing(
      map,
      currentStage = CurrentStage(0),
      definedStages = stages,
      housingState = upgradableHousingState.copy(commodityShortage = 10),
      areHousingCommoditiesMissing = true
    ) should be(StructureTransition.Downgrade)

    // multi-stage; downgrade (service shortage)
    TransitionCalculations.housing(
      map,
      currentStage = CurrentStage(0),
      definedStages = stages,
      housingState = upgradableHousingState.copy(
        education = Map(
          EducationEntry("Entry#1") -> EducationLevel(current = 4, minimal = 5, required = 10)
        )
      ),
      areHousingCommoditiesMissing = false
    ) should be(StructureTransition.Downgrade)

    // multi-stage; no transition (no occupants)
    TransitionCalculations.housing(
      map,
      currentStage = CurrentStage(0),
      definedStages = stages,
      housingState = upgradableHousingState.copy(occupants = 0),
      areHousingCommoditiesMissing = false
    ) should be(StructureTransition.None)

    // multi-stage; no transition (not needed)
    TransitionCalculations.housing(
      map,
      currentStage = CurrentStage(1),
      definedStages = stages,
      housingState = HousingState(
        occupants = 5,
        commodityShortage = 0,
        education = Map(
          EducationEntry("Entry#1") -> EducationLevel(current = 9, minimal = 0, required = 10)
        ),
        entertainment = Map(
          EntertainmentEntry("Entry#2") -> EntertainmentLevel(current = 9, minimal = 0, required = 10)
        ),
        religion = Map(
          ReligionEntry("Entry#3") -> ReligionLevel(current = 9, minimal = 0, required = 10)
        ),
        healthcare = Map(
          HealthcareEntry("Entry#4") -> HealthcareLevel(current = 9, minimal = 0, required = 10)
        ),
        civilService = Map(
          CivilServiceEntry("Entry#5") -> CivilServiceLevel(current = 9, minimal = 0, required = 10)
        )
      ),
      areHousingCommoditiesMissing = false
    ) should be(StructureTransition.None)
  }

  it should "calculate producing structure transitions" in { _ =>
    val map = Fixtures.defaultMapData.copy(
      cellState = Fixtures.defaultCellState.copy(desirability = CellDesirability(4))
    )

    val stages = MultiStage(
      stages = Seq(
        StageProperties(
          maxLife = Life(100),
          maxPeople = 5,
          minDesirability = CellDesirability.Neutral,
          commodityShortageLimit = 10
        ),
        StageProperties(
          maxLife = Life(150),
          maxPeople = 15,
          minDesirability = CellDesirability(4),
          commodityShortageLimit = 5
        ),
        StageProperties(
          maxLife = Life(200),
          maxPeople = 50,
          minDesirability = CellDesirability.Max,
          commodityShortageLimit = 3
        )
      )
    )

    // single stage; no transition
    TransitionCalculations.producing(
      map,
      currentStage = DefaultStage,
      definedStages = SingleStage(
        stage = StageProperties(
          maxLife = Life(100),
          maxPeople = 15,
          minDesirability = CellDesirability.Neutral,
          commodityShortageLimit = 0
        )
      )
    ) should be(StructureTransition.None)

    // multi-stage; upgrade
    TransitionCalculations.producing(
      map,
      currentStage = CurrentStage(0),
      definedStages = stages
    ) should be(StructureTransition.Upgrade)

    // multi-stage; downgrade
    TransitionCalculations.producing(
      map,
      currentStage = CurrentStage(2),
      definedStages = stages
    ) should be(StructureTransition.Downgrade)

    // multi-stage; no transition (not needed)
    TransitionCalculations.producing(
      map,
      currentStage = CurrentStage(1),
      definedStages = stages
    ) should be(StructureTransition.None)
  }
}
