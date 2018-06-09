package owe.test.specs.unit.entities.active.behaviour.structure

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class CommodityCalculationsSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "CommodityCalculations" should "calculate production" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "calculate consumption" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "calculate required commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "calculate missing commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
