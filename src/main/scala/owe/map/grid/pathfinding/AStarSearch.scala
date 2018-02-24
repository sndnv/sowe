package owe.map.grid.pathfinding

import scala.annotation.tailrec
import scala.collection.mutable
import owe.map.grid.Point

import scala.collection.immutable.Queue

object AStarSearch extends Search {
  private case class PathData(point: Point, actualCost: Double, expectedCost: Double, path: Queue[Point])
  private def orderingByExpectedCost[A <: PathData]: Ordering[A] = Ordering.by(_.expectedCost)

  def heuristic(point: Point, goal: Point): Double = {
    Math.abs(goal.x - point.x) + Math.abs(goal.y - point.y)
  }

  override def calculate(start: Point, goal: Point, neighbours: Point => Seq[Point]): Option[Queue[Point]] = {
    val available: mutable.PriorityQueue[PathData] =
      mutable.PriorityQueue.empty[PathData](orderingByExpectedCost.reverse)
    available.enqueue(PathData(start, 0, heuristic(start, goal), Queue(start)))

    @tailrec
    def calculate(visited: Queue[Point]): Option[Queue[Point]] = {
      if (available.nonEmpty) {
        val PathData(point, actualCost, _, path) = available.dequeue()

        if (point == goal) {
          Some(path)
        } else if (!visited.contains(point)) {
          neighbours(point).foreach { neighbour =>
            if (!visited.contains(neighbour)) {
              available.enqueue(
                PathData(neighbour, actualCost + 1, actualCost + heuristic(neighbour, goal), path :+ neighbour)
              )
            }
          }
          calculate(visited.enqueue(point))
        } else {
          calculate(visited)
        }
      } else {
        None
      }
    }

    calculate(Queue.empty)
  }
}
