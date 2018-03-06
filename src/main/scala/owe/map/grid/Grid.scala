package owe.map.grid

import scala.reflect.ClassTag
import scala.util.Try

class Grid[A: ClassTag](private val data: Array[Array[A]]) {

  val width: Int = data.headOption.map(_.length).getOrElse(0)

  val height: Int = data.length

  def map[B: ClassTag](f: A => B): Grid[B] = new Grid(data.map(_.map(f).toArray))

  def mapIndexed[B: ClassTag](f: (Point, A) => B): Grid[B] =
    new Grid(
      data.zipWithIndex.map {
        case (row, rowIndex) =>
          row.zipWithIndex.map {
            case (col, colIndex) =>
              f((rowIndex, colIndex), col)
          }
      }
    )

  def foreach[U](f: A => U): Unit = data.foreach(_.foreach(f))

  def foreachIndexed[U](f: (Point, A) => U): Unit =
    data.zipWithIndex.foreach {
      case (row, rowIndex) =>
        row.zipWithIndex.foreach {
          case (col, colIndex) =>
            f((rowIndex, colIndex), col)
        }
    }

  def indexes(): Grid[Point] = mapIndexed { case (point, _) => point }

  def slice(cols: Range, rows: Range): Grid[A] =
    new Grid(
      cols.map { x =>
        rows.flatMap { y =>
          row(y).map(_(x))
        }.toArray
      }.toArray
    )

  def colUnsafe(x: Int): Seq[A] = data.map(_(x))

  def rowUnsafe(y: Int): Seq[A] = data(y)

  def col(x: Int): Option[Seq[A]] = Try(colUnsafe(x)).toOption

  def row(y: Int): Option[Seq[A]] = Try(rowUnsafe(y)).toOption

  def cols: Seq[Seq[A]] = data.transpose.map(_.toSeq)

  def rows: Seq[Seq[A]] = data.map(_.toSeq)

  def cols(cols: Range): Seq[Seq[A]] = cols.flatMap(col)

  def rows(rows: Range): Seq[Seq[A]] = rows.flatMap(row)

  def table: Seq[Seq[A]] = rows

  def toSeq: Seq[A] = data.flatMap(_.toSeq)

  def getUnsafe(point: Point): A = data(point.x)(point.y)

  def get(point: Point): Option[A] = Try(getUnsafe(point)).toOption

  def updated(point: Point, element: A): Grid[A] = {
    val result = map(identity)
    result.data(point.x)(point.y) = element
    result
  }

  def forall(p: A => Boolean): Boolean = data.forall(_.forall(p))

  def exists(p: A => Boolean): Boolean = data.exists(_.exists(p))

  def find(p: A => Boolean): Option[A] = data.collectFirst { case row => row.find(p) }.flatten

  def findRow(p: A => Boolean): Option[Seq[A]] = rows.find(_.exists(p))

  def findCol(p: A => Boolean): Option[Seq[A]] = cols.find(_.exists(p))

  def filter(p: A => Boolean): Seq[A] = data.flatMap(_.filter(p))

  def collect[B: ClassTag](pf: PartialFunction[A, B]): Seq[B] = data.flatMap(_.collect(pf))

  def window(point: Point, radius: Int): Grid[A] = {
    val cols = Math.max(point.x - radius, 0) to Math.min(point.x + radius, width - 1)
    val rows = Math.max(point.y - radius, 0) to Math.min(point.y + radius, height - 1)

    slice(rows, cols)
  }

  def slide[U](start: Point, end: Point, radius: Int, f: A => U): Unit =
    mapIndexed { case (p, _) => p }
      .slice(start.x to end.x, start.y to end.y)
      .toSeq
      .foreach { point =>
        window(point, radius).foreach(f)
      }

  def slide[U](start: Point, radius: Int, f: A => U): Unit =
    slide(start, end = Point(Math.max(width - 1, 0), Math.max(height - 1, 0)), radius, f)

  def slide[U](radius: Int, f: A => U): Unit = slide(start = Point(0, 0), radius, f)

  def transpose: Grid[A] = new Grid(data.transpose)

  def debugString(): String = {
    val header = s" \t${(0 until width).mkString("\t")}\tX"
    val headerSep = s" \t${(0 until width).map(_ => "--").mkString("\t")}"

    val body = table.zipWithIndex
      .map {
        case (row, index) =>
          s"$index|\t${row.mkString("\t")}"
      }
      .mkString("\n")

    val footerSep = s"Y \t${(0 until width).map(_ => "--").mkString("\t")}"

    s"$header\n$headerSep\n$body\n$footerSep"
  }
}

object Grid {
  def apply[A: ClassTag](width: Int, height: Int, f: => A): Grid[A] = new Grid(Array.fill[A](height, width)(f))

  def apply[A: ClassTag](size: Int, f: => A): Grid[A] = Grid(size, size, f)
}
