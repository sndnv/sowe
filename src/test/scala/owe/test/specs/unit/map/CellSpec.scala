package owe.test.specs.unit.map

import org.scalatest.FutureOutcome
import owe.test.specs.unit.AsyncUnitSpec

class CellSpec extends AsyncUnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFixture(test.toNoArgAsyncTest(FixtureParam()))

  "A Cell" should "add and remove entities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "update its modifiers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "return cell data and availability information" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
