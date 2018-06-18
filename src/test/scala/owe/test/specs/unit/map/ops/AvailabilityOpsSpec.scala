package owe.test.specs.unit.map.ops

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class AvailabilityOpsSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Availability ops" should "calculate cell availability" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "find adjacent roads" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
