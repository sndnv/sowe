package owe.test.specs.unit.map.grid.pathfinding

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class SearchSpec extends UnitSpec {

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An A* search" should "find available paths in a grid" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "fail when a path is not available" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  "A Breadth-First search" should "find available paths in a grid" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "fail when a path is not available" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  "A Depth-First search" should "find available paths in a grid" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "fail when a path is not available" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
