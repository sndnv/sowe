package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ProcessedFarmingSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A ProcessedFarming transformation" should "update a structure's commodities based on production" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
