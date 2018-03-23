package owe.test.specs.unit.production

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ExchangeSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An Exchange" should "update required commodities" in { _ =>
    //TODO
  }

  it should "update available commodities" in { _ =>
    //TODO
  }

  it should "update in-transit commodities" in { _ =>
    //TODO
  }

  it should "update state of commodities" in { _ =>
    //TODO
  }

  it should "add producers" in { _ =>
    //TODO
  }

  it should "remove producers" in { _ =>
    //TODO
  }

  it should "add consumers" in { _ =>
    //TODO
  }

  it should "remove consumers" in { _ =>
    //TODO
  }
}
