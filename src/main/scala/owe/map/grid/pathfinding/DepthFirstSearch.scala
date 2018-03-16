package owe.map.grid.pathfinding
import owe.map.grid.Point

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object DepthFirstSearch extends Search {
  private case class PathData(point: Point, path: Queue[Point])

  override def calculate(start: Point, goal: Point, neighbours: Point => Seq[Point]): Option[Queue[Point]] = {
    @tailrec
    def calculate(available: Vector[PathData], visited: Queue[Point]): Option[Queue[Point]] =
      available.lastOption match {
        case Some(PathData(point, path)) =>
          if (point == goal) {
            Some(path)
          } else if (!visited.contains(point)) {
            val paths = neighbours(point).collect {
              case neighbour if !visited.contains(neighbour) =>
                PathData(neighbour, path :+ neighbour)
            }
            calculate(available.dropRight(1) ++ paths, visited.enqueue(point))
          } else {
            calculate(available.dropRight(1), visited)
          }

        case None => None
      }

    calculate(Vector(PathData(start, path = Queue(start))), Queue.empty)
  }
}
