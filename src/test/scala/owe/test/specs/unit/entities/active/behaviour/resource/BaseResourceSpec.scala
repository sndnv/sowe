package owe.test.specs.unit.entities.active.behaviour.resource

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class BaseResourceSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A BaseResource" should "produce commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "allow commodities to be retrieved by walkers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
