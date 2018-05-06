package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class BaseWalkerSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A BaseWalker " should "accept player instructions" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not pass through roadblocks when appropriate" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not pass through enemy units" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "halt movement if no clear path is available" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "halt movement if target cannot accept resources" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
