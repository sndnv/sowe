package owe.test.specs.unit.map.grid.pathfinding

import org.scalatest.Outcome
import owe.map.grid.Point
import owe.map.grid.pathfinding.AStarSearch
import owe.test.specs.unit.UnitSpec

import scala.collection.immutable.Queue

class AStarSearchSpec extends UnitSpec {

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An A* search" should "find available paths in a grid" in { _ =>
    AStarSearch.calculate(
      (0, 0),
      (0, 0),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0))))

    AStarSearch.calculate(
      (0, 0),
      (1, 0),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 0))))

    AStarSearch.calculate(
      (0, 0),
      (2, 0),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 0), (2, 0))))

    AStarSearch.calculate(
      (0, 0),
      (0, 1),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (0, 1))))

    AStarSearch.calculate(
      (0, 0),
      (1, 1),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1))))

    AStarSearch.calculate(
      (0, 0),
      (2, 1),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (2, 1))))

    AStarSearch.calculate(
      (0, 0),
      (0, 2),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (0, 1), (0, 2))))

    AStarSearch.calculate(
      (0, 0),
      (1, 2),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (1, 2))))

    AStarSearch.calculate(
      (0, 0),
      (2, 2),
      neighboursOf
    ) should be(Some(Queue[Point]((0, 0), (1, 1), (2, 2))))
  }

  it should "fail when a path is not available" in { _ =>
    AStarSearch.calculate(
      (0, 0),
      (3, 0),
      neighboursOf
    ) should be(None)

    AStarSearch.calculate(
      (0, -1),
      (1, 0),
      neighboursOf
    ) should be(None)

    AStarSearch.calculate(
      (4, 0),
      (0, 0),
      neighboursOf
    ) should be(None)
  }
}
