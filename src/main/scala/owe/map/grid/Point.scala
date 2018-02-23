package owe.map.grid

import scala.language.implicitConversions

case class Point(x: Int, y: Int)

object Point {
  implicit def tupleToLocation(t: (Int, Int)): Point = Point(t._1, t._2)
}
