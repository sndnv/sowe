package owe.test.specs.unit.entities.active.behaviour.structure

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class BaseStructureSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A BaseStructure" should "support handling production structures" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "support handling farming structures" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "support handling housing structures" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
