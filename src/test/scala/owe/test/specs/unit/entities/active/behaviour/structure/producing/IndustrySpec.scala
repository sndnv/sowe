package owe.test.specs.unit.entities.active.behaviour.structure.producing

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class IndustrySpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An Industry structure" should "generate carrier walkers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "generate recruiters" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "accept commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "transfer commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
