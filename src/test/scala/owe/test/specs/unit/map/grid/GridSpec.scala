package owe.test.specs.unit.map.grid

import scala.util.Success

import akka.actor.FSM.Failure
import org.scalatest.Outcome
import owe.map.grid.{Grid, Point}
import owe.test.specs.unit.UnitSpec

class GridSpec extends UnitSpec {

  case class FixtureParam(grid: Grid[Int])

  private class Box(var value: Int = 0)

  def withFixture(test: OneArgTest): Outcome = {
    val size = 3
    val grid = Grid(size, f = 0).mapIndexed((p, _) => p.x + p.y * size)

    withFixture(test.toNoArgTest(FixtureParam(grid)))
  }

  "A Grid" should "iterate over stored elements" in { fixture =>
    fixture.grid.toMap should be(
      Map(
        Point(0, 0) -> 0,
        Point(1, 0) -> 1,
        Point(2, 0) -> 2,
        Point(0, 1) -> 3,
        Point(1, 1) -> 4,
        Point(2, 1) -> 5,
        Point(0, 2) -> 6,
        Point(1, 2) -> 7,
        Point(2, 2) -> 8
      )
    )

    fixture.grid.toSeq should be(0 to 8)

    val mutableGrid = Grid(size = 3, new Box())
    mutableGrid.foreachIndexed((p, box) => box.value = p.x + p.y)
    mutableGrid.map(_.value).toMap should be(
      Map(
        Point(0, 0) -> 0,
        Point(1, 0) -> 1,
        Point(2, 0) -> 2,
        Point(0, 1) -> 1,
        Point(1, 1) -> 2,
        Point(2, 1) -> 3,
        Point(0, 2) -> 2,
        Point(1, 2) -> 3,
        Point(2, 2) -> 4
      )
    )
  }

  it should "create slices" in { fixture =>
    fixture.grid.slice(0 to 2, 0 to 2).toMap should be(fixture.grid.toMap)

    fixture.grid.indexed().slice(2 to 2, 0 to 1).toMap should be(
      Map(
        Point(0, 0) -> (Point(0, 2), 6),
        Point(1, 0) -> (Point(1, 2), 7)
      )
    )

    fixture.grid.indexed().slice(0 to 2, 1 to 2).toMap should be(
      Map(
        Point(0, 0) -> (Point(1, 0), 1),
        Point(1, 0) -> (Point(2, 0), 2),
        Point(0, 1) -> (Point(1, 1), 4),
        Point(1, 1) -> (Point(2, 1), 5),
        Point(0, 2) -> (Point(1, 2), 7),
        Point(1, 2) -> (Point(2, 2), 8)
      )
    )
  }

  it should "access individual rows and columns" in { fixture =>
    fixture.grid.col(2) should be(Some(Seq(2, 5, 8)))

    fixture.grid.col(5) should be(None)

    fixture.grid.row(0) should be(Some(Seq(0, 1, 2)))

    fixture.grid.row(19) should be(None)
  }

  it should "access groups of rows and columns" in { fixture =>
    fixture.grid.cols should be(
      Seq(
        Seq(0, 3, 6),
        Seq(1, 4, 7),
        Seq(2, 5, 8)
      )
    )

    fixture.grid.rows should be(
      Seq(
        Seq(0, 1, 2),
        Seq(3, 4, 5),
        Seq(6, 7, 8)
      )
    )

    fixture.grid.cols(1 to 5) should be(
      Seq(
        Seq(1, 4, 7),
        Seq(2, 5, 8)
      )
    )

    fixture.grid.rows(0 to 1) should be(
      Seq(
        Seq(0, 1, 2),
        Seq(3, 4, 5)
      )
    )

    fixture.grid.table should be(
      Seq(
        Seq(0, 1, 2),
        Seq(3, 4, 5),
        Seq(6, 7, 8)
      )
    )
  }

  it should "access individual elements" in { fixture =>
    fixture.grid.get(Point(0, 0)) should be(Some(0))
    fixture.grid.get(Point(1, 0)) should be(Some(1))
    fixture.grid.get(Point(2, 0)) should be(Some(2))
    fixture.grid.get(Point(0, 1)) should be(Some(3))
    fixture.grid.get(Point(1, 1)) should be(Some(4))
    fixture.grid.get(Point(2, 1)) should be(Some(5))
    fixture.grid.get(Point(0, 2)) should be(Some(6))
    fixture.grid.get(Point(1, 2)) should be(Some(7))
    fixture.grid.get(Point(2, 2)) should be(Some(8))
  }

  it should "update individual elements" in { fixture =>
    fixture.grid
      .updated((1, 2), 42)
      .updated((2, 1), 83)
      .toMap should be(
      Map(
        Point(0, 0) -> 0,
        Point(1, 0) -> 1,
        Point(2, 0) -> 2,
        Point(0, 1) -> 3,
        Point(1, 1) -> 4,
        Point(2, 1) -> 83,
        Point(0, 2) -> 6,
        Point(1, 2) -> 42,
        Point(2, 2) -> 8
      )
    )

    fixture.grid
      .updated(Map(Point(1, 2) -> 42, Point(2, 1) -> 83))
      .toMap should be(
      Map(
        Point(0, 0) -> 0,
        Point(1, 0) -> 1,
        Point(2, 0) -> 2,
        Point(0, 1) -> 3,
        Point(1, 1) -> 4,
        Point(2, 1) -> 83,
        Point(0, 2) -> 6,
        Point(1, 2) -> 42,
        Point(2, 2) -> 8
      )
    )
  }

  it should "be rebuild-able from a map" in { fixture =>
    val targetMap = Map(
      Point(0, 0) -> "1",
      Point(1, 0) -> "2",
      Point(2, 0) -> "3",
      Point(0, 1) -> "4",
      Point(1, 1) -> "5",
      Point(2, 1) -> "6",
      Point(0, 2) -> "7",
      Point(1, 2) -> "8",
      Point(2, 2) -> "9"
    )

    fixture.grid
      .rebuilt(targetMap)
      .map(_.toMap) should be(
      Success(targetMap)
    )

    assertThrows[IllegalArgumentException] {
      fixture.grid
        .rebuilt(Map.empty[Point, String])
        .map(_.toMap)
        .get
    }
  }

  it should "search for elements" in { fixture =>
    fixture.grid.hasPoint(Point(1, 1)) should be(true)
    fixture.grid.hasPoint(Point(3, 1)) should be(false)
    fixture.grid.forall(_ >= 0) should be(true)
    fixture.grid.exists(_ == 7) should be(true)
    fixture.grid.find(_ == 7) should be(Some(7))
    fixture.grid.filter(_ > 5) should be(Seq(6, 7, 8))
    fixture.grid.findRow(_ == 7) should be(Some(Seq(6, 7, 8)))
    fixture.grid.findCol(_ == 7) should be(Some(Seq(1, 4, 7)))
    fixture.grid.collect { case i if i % 2 == 0 => i.toString } should be(Seq("0", "2", "4", "6", "8"))
  }

  it should "access elements with a sliding window" in { _ =>
    val mutableGrid = Grid(size = 3, new Box())
    mutableGrid.sliding(1, box => box.value += 1)
    mutableGrid.map(_.value).toMap should be(
      Map(
        Point(0, 0) -> 4,
        Point(1, 0) -> 6,
        Point(2, 0) -> 4,
        Point(0, 1) -> 6,
        Point(1, 1) -> 9,
        Point(2, 1) -> 6,
        Point(0, 2) -> 4,
        Point(1, 2) -> 6,
        Point(2, 2) -> 4
      )
    )
  }

  it should "retrieve the next logical point in the grid" in { fixture =>
    val indexes = fixture.grid.indexes().toSeq
    indexes.zip(indexes.tail :+ indexes.head).foreach {
      case (current, next) =>
        fixture.grid.nextPoint(current) should be(Some(next))
    }
  }

  it should "transpose elements" in { fixture =>
    fixture.grid.transposed.toMap should be(
      Grid(fixture.grid.height, f = 0).mapIndexed((p, _) => p.x * fixture.grid.height + p.y).toMap
    )
  }

  it should "generate grids as strings" in { fixture =>
    fixture.grid.debugString() should be(
      """ 	0	1	2	X
        | 	--	--	--
        |0|	0	1	2
        |1|	3	4	5
        |2|	6	7	8
        |Y 	--	--	--""".stripMargin
    )
  }
}
