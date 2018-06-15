package owe.test.specs.unit.map.ops

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class PathingOpsSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Pathing ops" should "calculate passable neighbours" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "generate advance paths" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "generate roaming paths" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}