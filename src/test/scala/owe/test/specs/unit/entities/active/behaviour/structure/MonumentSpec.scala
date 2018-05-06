package owe.test.specs.unit.entities.active.behaviour.structure

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class MonumentSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Monument structure" should "generate recruiters" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "accept commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "upgrade itself based on conditions and available resources" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
