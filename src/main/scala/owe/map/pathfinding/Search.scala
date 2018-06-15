package owe.map.pathfinding

import owe.map.grid.Point

import scala.collection.immutable.Queue

trait Search {
  def calculate(start: Point, goal: Point, neighbours: Point => Seq[Point]): Option[Queue[Point]]
}
