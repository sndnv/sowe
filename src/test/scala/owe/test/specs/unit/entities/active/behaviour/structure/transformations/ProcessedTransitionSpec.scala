package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ProcessedTransitionSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A ProcessedTransition transformation" should "upgrade a structure" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "downgrade a structure" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "keep a structure's stage" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
