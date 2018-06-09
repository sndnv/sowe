package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class ProcessedHousingSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A ProcessedHousing transformation" should "handle education updates" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "handle entertainment updates" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "handle religion updates" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "handle healthcare updates" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "handle civil service updates" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "handle commodity shortages" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
