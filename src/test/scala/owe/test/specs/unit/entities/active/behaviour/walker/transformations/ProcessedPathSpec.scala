package owe.test.specs.unit.entities.active.behaviour.walker.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ProcessedPathSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A ProcessedPath transformation" should "update a walker's path" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
