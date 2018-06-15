package owe.test.specs.unit.map.ops

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class EntityOpsSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Entity ops" should "create entities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "destroy entities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "move entities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
