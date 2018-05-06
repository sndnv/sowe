package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class UndesirableSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An Undesirable walker" should "roam around" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "attack if any enemies are in range" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "go to a destination and perform action" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
