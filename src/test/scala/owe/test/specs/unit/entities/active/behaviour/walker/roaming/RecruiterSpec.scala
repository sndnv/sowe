package owe.test.specs.unit.entities.active.behaviour.walker.roaming

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class RecruiterSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Recruiter walker" should "roam around until labour is found" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "return home when max distance is covered" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
