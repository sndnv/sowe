package owe.test.specs.unit.entities.active.behaviour.walker.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ProcessedMovementSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A ProcessedMovement transformation" should "increase a walker's covered distance" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "update a walker's movement mode" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
