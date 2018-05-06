package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class BaseDistributorSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A BaseDistributor" should "calculate transfers from walkers to structures" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "calculate transfers from structures to walkers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
