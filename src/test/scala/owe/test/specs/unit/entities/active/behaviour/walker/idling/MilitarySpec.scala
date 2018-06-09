package owe.test.specs.unit.entities.active.behaviour.walker.idling

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class MilitarySpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Military walker" should "go to a destination and stay" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "attack if any enemies are in range" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "be able to follow other military walkers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
