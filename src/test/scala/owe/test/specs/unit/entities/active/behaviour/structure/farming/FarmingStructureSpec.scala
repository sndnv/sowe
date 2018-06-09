package owe.test.specs.unit.entities.active.behaviour.structure.farming

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class FarmingStructureSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Farming structure" should "generate walkers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "be affected by map fertility and water levels" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "transfer commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
