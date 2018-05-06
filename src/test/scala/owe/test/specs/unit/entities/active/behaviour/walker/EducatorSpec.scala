package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class EducatorSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An Educator walker" should "roam around" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "return home when max distance is covered" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
