package owe.test.specs.unit.entities.active.behaviour.structure.producing

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ProducingStructureSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Producing structure" should "generate walkers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "accept commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "consume commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "upgrade itself based on conditions" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "downgrade itself based on conditions" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
