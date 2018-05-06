package owe.test.specs.unit.events

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class TrackerSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Tracker" should "attach observers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "forward events to observers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "allow event querying" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "allow event clearing" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "detach observers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
