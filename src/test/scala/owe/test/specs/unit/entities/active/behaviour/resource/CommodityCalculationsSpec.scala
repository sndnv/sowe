package owe.test.specs.unit.entities.active.behaviour.resource

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class CommodityCalculationsSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "CommodityCalculations" should "calculate amount produced" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
