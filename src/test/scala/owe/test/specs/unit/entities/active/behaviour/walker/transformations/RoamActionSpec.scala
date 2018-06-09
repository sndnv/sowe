package owe.test.specs.unit.entities.active.behaviour.walker.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class RoamActionSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A RoamAction transformation" should "process actions" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
