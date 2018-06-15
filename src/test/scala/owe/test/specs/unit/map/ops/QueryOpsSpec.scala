package owe.test.specs.unit.map.ops

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class QueryOpsSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Query ops" should "retrieve paths" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "retrieve neighbours" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "retrieve entities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
