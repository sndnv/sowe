package owe

import scala.language.implicitConversions

package object map {
  case class Location(x: Int, y: Int)

  implicit def tupleToLocation(t: (Int, Int)): Location = Location(t._1, t._2)
}
