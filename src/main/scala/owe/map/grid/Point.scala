package owe.map.grid

import scala.language.implicitConversions

case class Point(x: Int, y: Int)

object Point {
  implicit def tupleToPoint(t: (Int, Int)): Point = Point(t._1, t._2)
}
