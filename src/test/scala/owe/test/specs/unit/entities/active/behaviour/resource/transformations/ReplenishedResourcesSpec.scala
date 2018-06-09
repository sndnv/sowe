package owe.test.specs.unit.entities.active.behaviour.resource.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ReplenishedResourcesSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A ReplenishedResources transformation" should "add amount produced" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
