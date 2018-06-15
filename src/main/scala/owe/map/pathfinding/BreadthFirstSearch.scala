package owe.map.pathfinding
import owe.map.grid.Point

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object BreadthFirstSearch extends Search {
  private case class PathData(point: Point, path: Queue[Point])

  override def calculate(start: Point, goal: Point, neighbours: Point => Seq[Point]): Option[Queue[Point]] = {
    @tailrec
    def calculate(available: Queue[PathData], visited: Queue[Point]): Option[Queue[Point]] =
      available.dequeueOption match {
        case Some((PathData(point, path), remaining)) =>
          if (point == goal) {
            Some(path)
          } else if (!visited.contains(point)) {
            val paths = neighbours(point).collect {
              case neighbour if !visited.contains(neighbour) =>
                PathData(neighbour, path :+ neighbour)
            }
            calculate(remaining.enqueue(paths.toList), visited.enqueue(point))
          } else {
            calculate(remaining, visited)
          }

        case None => None
      }

    calculate(Queue(PathData(start, path = Queue(start))), Queue.empty)
  }
}
