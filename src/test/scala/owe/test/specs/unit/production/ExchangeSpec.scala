package owe.test.specs.unit.production

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ExchangeSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An Exchange" should "update required commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "update available commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "update in-transit commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "update state of commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "add producers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "remove producers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "add consumers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "remove consumers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
