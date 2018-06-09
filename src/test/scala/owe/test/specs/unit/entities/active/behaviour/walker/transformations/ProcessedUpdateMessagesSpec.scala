package owe.test.specs.unit.entities.active.behaviour.walker.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ProcessedUpdateMessagesSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A ProcessedUpdateMessages transformation" should "handle incoming and outgoing commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "handle attacks" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
