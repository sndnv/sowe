package owe.test.specs.unit.map.ops

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ForwardingOpsSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Forwarding ops" should "forward messages" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
