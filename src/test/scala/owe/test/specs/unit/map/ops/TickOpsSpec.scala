package owe.test.specs.unit.map.ops

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class TickOpsSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Tick ops" should "gather active effects" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "process cells" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "process ticks" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
