package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class AnimalSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An Animal walker" should "roam around" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "attack if any enemies are in range" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "follow another walker when captured" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
