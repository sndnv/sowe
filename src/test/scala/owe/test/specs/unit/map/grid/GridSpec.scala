package owe.test.specs.unit.map.grid

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class GridSpec extends UnitSpec {

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(FixtureParam()))
  }

  "A Grid" should "map over stored elements" in { _ =>
    //TODO
  }

  it should "create slices" in { _ =>
    //TODO
  }

  it should "access individual rows and columns" in { _ =>
    //TODO
  }

  it should "access groups of rows and columns" in { _ =>
    //TODO
  }

  it should "access individual elements" in { _ =>
    //TODO
  }

  it should "update individual elements" in { _ =>
    //TODO
  }

  it should "search for elements" in { _ =>
    //TODO
  }

  it should "search for rows and columns" in { _ =>
    //TODO
  }

  it should "access elements with a sliding window" in { _ =>
    //TODO
  }

  it should "transpose elements" in { _ =>
    //TODO
  }
}
