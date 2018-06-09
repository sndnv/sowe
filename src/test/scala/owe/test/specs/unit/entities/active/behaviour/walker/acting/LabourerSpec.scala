package owe.test.specs.unit.entities.active.behaviour.walker.acting

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class LabourerSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Labourer walker" should "go to resources and gather commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "go to parent structure and offload commodities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "go to a destination and perform action" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
