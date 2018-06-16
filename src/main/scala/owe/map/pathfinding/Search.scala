package owe.map.pathfinding

import owe.map.grid.Point

import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}

trait Search {
  def calculate(
    start: Point,
    goal: Point,
    neighbours: Point => Future[Seq[Point]]
  )(implicit ec: ExecutionContext): Future[Queue[Point]]
}
