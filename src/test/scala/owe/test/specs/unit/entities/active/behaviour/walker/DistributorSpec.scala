package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class DistributorSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Distributor walker" should "roam around and distribute commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "go to parent structure when out of commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not over-provision resources when multiple distributors are active" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
