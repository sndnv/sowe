package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class CarrierSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Carrier walker" should "go to destination and (un)load commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "wait at destination if it cannot accept commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "return with commodities if source structure can accept them" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
