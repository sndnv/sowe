package owe.map.pathfinding
import owe.map.grid.Point

import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}

object BreadthFirstSearch extends Search {
  private case class PathData(point: Point, path: Queue[Point])

  override def calculate(
    start: Point,
    goal: Point,
    neighbours: Point => Future[Seq[Point]]
  )(implicit ec: ExecutionContext): Future[Queue[Point]] = {
    def calculate(available: Queue[PathData], visited: Queue[Point]): Future[Queue[Point]] =
      available.dequeueOption match {
        case Some((PathData(point, path), remaining)) =>
          if (point == goal) {
            Future.successful(path)
          } else if (!visited.contains(point)) {
            neighbours(point).flatMap { neighbours =>
              val paths = neighbours.collect {
                case neighbour if !visited.contains(neighbour) =>
                  PathData(neighbour, path :+ neighbour)
              }

              calculate(remaining.enqueue(paths.toList), visited.enqueue(point))
            }
          } else {
            calculate(remaining, visited)
          }

        case None =>
          Future.successful(Queue.empty)
      }

    calculate(Queue(PathData(start, path = Queue(start))), Queue.empty)
  }
}
