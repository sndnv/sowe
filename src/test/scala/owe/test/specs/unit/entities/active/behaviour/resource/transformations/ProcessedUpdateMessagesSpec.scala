package owe.test.specs.unit.entities.active.behaviour.resource.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ProcessedUpdateMessagesSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A ProcessedUpdateMessages transformation" should "handle outgoing commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
