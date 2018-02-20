package owe.test.specs.unit.map

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class CursorSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(FixtureParam()))
  }

  "A Cursor" should "move up" in { _ =>
    //TODO
  }

  it should "move down" in { _ =>
    //TODO
  }

  it should "move left" in { _ =>
    //TODO
  }

  it should "move right" in { _ =>
    //TODO
  }
}
