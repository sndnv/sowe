package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class TraderSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Trader walker" should "go to destination and load/unload commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "wait at a dock until it is free or resources have been (un)loaded" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
