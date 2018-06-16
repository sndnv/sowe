package owe.test.specs.unit.map.pathfinding

import org.scalatest.FutureOutcome
import owe.map.grid.Point
import owe.map.pathfinding.AStarSearch
import owe.test.specs.unit.AsyncUnitSpec

import scala.collection.immutable.Queue

class AStarSearchSpec extends AsyncUnitSpec {

  case class FixtureParam()

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFixture(test.toNoArgAsyncTest(FixtureParam()))

  "An A* search" should "find available paths in a grid" in { _ =>
    for {
      _ <- AStarSearch
        .calculate(
          (0, 0),
          (0, 0),
          neighboursOf
        )
        .map(_ should be(Queue[Point]((0, 0))))

      _ <- AStarSearch
        .calculate(
          (0, 0),
          (1, 0),
          neighboursOf
        )
        .map(_ should be(Queue[Point]((0, 0), (1, 0))))

      _ <- AStarSearch
        .calculate(
          (0, 0),
          (2, 0),
          neighboursOf
        )
        .map(_ should be(Queue[Point]((0, 0), (1, 0), (2, 0))))

      _ <- AStarSearch
        .calculate(
          (0, 0),
          (0, 1),
          neighboursOf
        )
        .map(_ should be(Queue[Point]((0, 0), (0, 1))))

      _ <- AStarSearch
        .calculate(
          (0, 0),
          (1, 1),
          neighboursOf
        )
        .map(_ should be(Queue[Point]((0, 0), (1, 1))))

      _ <- AStarSearch
        .calculate(
          (0, 0),
          (2, 1),
          neighboursOf
        )
        .map(_ should be(Queue[Point]((0, 0), (1, 1), (2, 1))))

      _ <- AStarSearch
        .calculate(
          (0, 0),
          (0, 2),
          neighboursOf
        )
        .map(_ should be(Queue[Point]((0, 0), (0, 1), (0, 2))))

      _ <- AStarSearch
        .calculate(
          (0, 0),
          (1, 2),
          neighboursOf
        )
        .map(_ should be(Queue[Point]((0, 0), (1, 1), (1, 2))))

      _ <- AStarSearch
        .calculate(
          (0, 0),
          (2, 2),
          neighboursOf
        )
        .map(_ should be(Queue[Point]((0, 0), (1, 1), (2, 2))))
    } yield {
      succeed
    }
  }

  it should "fail when a path is not available" in { _ =>
    for {
      _ <- AStarSearch
        .calculate(
          (0, 0),
          (3, 0),
          neighboursOf
        )
        .map(_ should be(Queue.empty))

      _ <- AStarSearch
        .calculate(
          (0, -1),
          (1, 0),
          neighboursOf
        )
        .map(_ should be(Queue.empty))

      _ <- AStarSearch
        .calculate(
          (4, 0),
          (0, 0),
          neighboursOf
        )
        .map(_ should be(Queue.empty))
    } yield {
      succeed
    }
  }
}
