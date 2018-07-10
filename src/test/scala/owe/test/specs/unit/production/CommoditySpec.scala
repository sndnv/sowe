package owe.test.specs.unit.production

import org.scalatest.Outcome
import owe.map.Cell.Fertility
import owe.production.Commodity.{Amount, AmountModifier}
import owe.test.specs.unit.UnitSpec

class CommoditySpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Commodity Amount" should "support math ops" in { _ =>
    Amount(50) + Amount(60) should be(Amount(110))
    Amount(50) - Amount(60) should be(Amount(-10))
    Amount(50) * 3 should be(Amount(150))
    Amount(50) / 2 should be(Amount(25))
  }

  it should "support comparison ops" in { _ =>
    Amount(50) > Amount(10) should be(true)
    Amount(50) < Amount(10) should be(false)
    Amount(50) >= Amount(10) should be(true)
    Amount(50) >= Amount(50) should be(true)
    Amount(50) <= Amount(10) should be(false)
    Amount(50) <= Amount(50) should be(true)
    Amount(50) == Amount(10) should be(false)
    Amount(50) == Amount(50) should be(true)
    Amount(50).min(Amount(10)) should be(Amount(10))
    Amount(50).max(Amount(10)) should be(Amount(50))
  }

  it should "be based on fertility" in { _ =>
    Amount(50).basedOn(Fertility(20)) should be(Amount(10))
  }

  "A Commodity Amount Modifier" should "support math ops" in { _ =>
    AmountModifier(50) + AmountModifier(60) should be(AmountModifier(110))
    AmountModifier(50) - AmountModifier(60) should be(AmountModifier(-10))
    AmountModifier(50) * 3 should be(AmountModifier(150))
    AmountModifier(50) / 2 should be(AmountModifier(25))
  }

  it should "be applied to a Commodity Amount" in { _ =>
    AmountModifier(50)(Amount(30)) should be(Amount(15))
  }
}
