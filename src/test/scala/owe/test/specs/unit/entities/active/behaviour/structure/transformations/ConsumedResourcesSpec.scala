package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ConsumedResourcesSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A ConsumedResources transformation" should "update a structure's commodities based on consumption" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
