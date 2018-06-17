package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure.{NoRisk, RiskState}
import owe.entities.active.behaviour.structure.transformations.ProcessedRisk
import owe.entities.active.{Life, RiskAmount}
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ProcessedRiskSpec extends UnitSpec {
  case class FixtureParam(
    transformer: ProcessedRisk,
    structure: StructureData
  )

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new ProcessedRisk {}

    val structure = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state,
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    withFixture(test.toNoArgTest(FixtureParam(transformer, structure)))
  }

  "A ProcessedRisk transformation" should "update a structure's risk state" in { fixture =>
    fixture.transformer.withProcessedRisk(
      structure = fixture.structure
    ) should be(fixture.structure.state.copy(risk = RiskState(fire = RiskAmount(3), damage = RiskAmount(5))))

    {
      val structureWithMaxRisk = fixture.structure.copy(
        state = fixture.structure.state.copy(
          risk = RiskState(fire = RiskAmount.max, damage = RiskAmount.max)
        )
      )

      fixture.transformer.withProcessedRisk(
        structureWithMaxRisk
      ) should be(
        structureWithMaxRisk.state.copy(currentLife = Life(0))
      )
    }

    {
      val structureWithNoRisk = fixture.structure.copy(
        state = fixture.structure.state.copy(risk = NoRisk)
      )

      fixture.transformer.withProcessedRisk(
        structureWithNoRisk
      ) should be(
        structureWithNoRisk.state
      )
    }
  }
}
