package owe.test.specs.unit.entities.active.behaviour.structure

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class MilitarySpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Military structure" should "generate military walkers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "generate recruiters" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "accept commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
