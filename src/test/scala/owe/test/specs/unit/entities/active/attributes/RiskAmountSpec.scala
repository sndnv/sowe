package owe.test.specs.unit.entities.active.attributes

import org.scalatest.Outcome
import owe.entities.active.attributes.RiskAmount
import owe.test.specs.unit.UnitSpec

class RiskAmountSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Risk Amount" should "support math ops" in { _ =>
    RiskAmount(50) + RiskAmount(10) should be(RiskAmount(60))
    RiskAmount(50) + RiskAmount(20) should be(RiskAmount(70))
    RiskAmount(50) + RiskAmount(30) should be(RiskAmount(80))
    RiskAmount(50) + RiskAmount(40) should be(RiskAmount(90))
    RiskAmount(50) + RiskAmount(50) should be(RiskAmount(100))
    RiskAmount(50) + RiskAmount(60) should be(RiskAmount(100))
    RiskAmount(50) + RiskAmount(70) should be(RiskAmount(100))
    RiskAmount(50) + RiskAmount(-10) should be(RiskAmount(40))
    RiskAmount(50) + RiskAmount(-20) should be(RiskAmount(30))
    RiskAmount(50) + RiskAmount(-30) should be(RiskAmount(20))
    RiskAmount(50) + RiskAmount(-40) should be(RiskAmount(10))
    RiskAmount(50) + RiskAmount(-50) should be(RiskAmount(0))
    RiskAmount(50) + RiskAmount(-60) should be(RiskAmount(0))
    RiskAmount(50) + RiskAmount(-70) should be(RiskAmount(0))
  }

  it should "support comparison ops" in { _ =>
    RiskAmount(50) > RiskAmount(10) should be(true)
    RiskAmount(50) < RiskAmount(10) should be(false)
    RiskAmount(50) >= RiskAmount(10) should be(true)
    RiskAmount(50) >= RiskAmount(50) should be(true)
    RiskAmount(50) <= RiskAmount(10) should be(false)
    RiskAmount(50) <= RiskAmount(50) should be(true)
    RiskAmount(50) == RiskAmount(10) should be(false)
    RiskAmount(50) == RiskAmount(50) should be(true)
    RiskAmount(50).min(RiskAmount(10)) should be(RiskAmount(10))
    RiskAmount(50).max(RiskAmount(10)) should be(RiskAmount(50))
  }

  it should "have pre-defined min and max constants" in { _ =>
    RiskAmount.min should be(RiskAmount(0))
    RiskAmount.max should be(RiskAmount(100))
  }
}
