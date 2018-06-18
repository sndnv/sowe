package owe.test.specs.unit.map.grid

import org.scalatest.Outcome
import owe.map.grid.Point
import owe.test.specs.unit.UnitSpec

class PointSpec extends UnitSpec {

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Point" should "be convertible from tuple" in { _ =>
    ((0, 0): Point) should be(Point(0, 0))
    ((1, 0): Point) should be(Point(1, 0))
    ((2, 0): Point) should be(Point(2, 0))
    ((0, 1): Point) should be(Point(0, 1))
    ((1, 1): Point) should be(Point(1, 1))
    ((2, 1): Point) should be(Point(2, 1))
    ((0, 2): Point) should be(Point(0, 2))
    ((1, 2): Point) should be(Point(1, 2))
    ((2, 2): Point) should be(Point(2, 2))
  }

  it should "calculate distance between points" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "have ordering" in { _ =>
    Seq[Point]((1, 2), (3, 14), (0, 0), (1, 1), (2, 0)).sorted should be(
      Seq[Point]((0, 0), (2, 0), (1, 1), (1, 2), (3, 14))
    )
  }
}
