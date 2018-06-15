package owe.test.specs.unit.map.pathfinding

import org.scalatest.Outcome
import owe.map.grid.Point
import owe.map.pathfinding.DepthFirstSearch
import owe.test.specs.unit.UnitSpec

import scala.collection.immutable.Queue

class DepthFirstSearchSpec extends UnitSpec {

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Depth-First search" should "find available paths in a grid" in { _ =>
    DepthFirstSearch.calculate(
      (0, 0),
      (0, 0),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0))))

    DepthFirstSearch.calculate(
      (0, 0),
      (1, 0),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (2, 2), (1, 2), (0, 2), (0, 1), (1, 0))))

    DepthFirstSearch.calculate(
      (0, 0),
      (2, 0),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (2, 2), (1, 2), (0, 2), (0, 1), (1, 0), (2, 1), (2, 0))))

    DepthFirstSearch.calculate(
      (0, 0),
      (0, 1),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (2, 2), (1, 2), (0, 2), (0, 1))))

    DepthFirstSearch.calculate(
      (0, 0),
      (1, 1),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1))))

    DepthFirstSearch.calculate(
      (0, 0),
      (2, 1),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (2, 2), (1, 2), (0, 2), (0, 1), (1, 0), (2, 1))))

    DepthFirstSearch.calculate(
      (0, 0),
      (0, 2),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (2, 2), (1, 2), (0, 2))))

    DepthFirstSearch.calculate(
      (0, 0),
      (1, 2),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (2, 2), (1, 2))))

    DepthFirstSearch.calculate(
      (0, 0),
      (2, 2),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (2, 2))))
  }

  it should "fail when a path is not available" in { _ =>
    DepthFirstSearch.calculate(
      (0, 0),
      (3, 0),
      neighboursOf
    ) should be(None)

    DepthFirstSearch.calculate(
      (0, -1),
      (1, 0),
      neighboursOf
    ) should be(None)

    DepthFirstSearch.calculate(
      (4, 0),
      (0, 0),
      neighboursOf
    ) should be(None)
  }
}
