package owe.map.pathfinding

import owe.map.grid.Point

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

object AStarSearch extends Search {
  private case class PathData(point: Point, actualCost: Double, expectedCost: Double, path: Queue[Point])
  private def orderingByExpectedCost[A <: PathData]: Ordering[A] = Ordering.by(_.expectedCost)

  def heuristic(point: Point, goal: Point): Double =
    Math.abs(goal.x - point.x) + Math.abs(goal.y - point.y)

  override def calculate(
    start: Point,
    goal: Point,
    neighbours: Point => Future[Seq[Point]]
  )(implicit ec: ExecutionContext): Future[Queue[Point]] = {

    def calculate(available: mutable.PriorityQueue[PathData], visited: Queue[Point]): Future[Queue[Point]] =
      if (available.nonEmpty) {
        val PathData(point, actualCost, _, path) = available.dequeue()

        if (point == goal) {
          Future.successful(path)
        } else if (!visited.contains(point)) {
          neighbours(point).flatMap { neighbours =>
            neighbours.foreach { neighbour =>
              if (!visited.contains(neighbour)) {
                available.enqueue(
                  PathData(neighbour, actualCost + 1, actualCost + heuristic(neighbour, goal), path :+ neighbour)
                )
              }
            }

            calculate(available, visited.enqueue(point))
          }
        } else {
          calculate(available, visited)
        }
      } else {
        Future.successful(Queue.empty)
      }

    val available: mutable.PriorityQueue[PathData] =
      mutable.PriorityQueue.empty[PathData](orderingByExpectedCost.reverse)

    available.enqueue(PathData(start, 0, heuristic(start, goal), Queue(start)))

    calculate(available, Queue.empty)
  }
}
