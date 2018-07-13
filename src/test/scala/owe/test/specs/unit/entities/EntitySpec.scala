package owe.test.specs.unit.entities

import org.scalatest.Outcome
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.map.Cell
import owe.map.grid.Point
import owe.test.specs.unit.UnitSpec

class EntitySpec extends UnitSpec {

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An Entity" should "be able to calculate entity cells" in { _ =>
    Entity.cells(
      Entity.Size(1, 1),
      Point(0, 0)
    ) should be(Seq[Point]((0, 0)))

    Entity.cells(
      Entity.Size(1, 1),
      Point(1, 0)
    ) should be(Seq[Point]((1, 0)))

    Entity.cells(
      Entity.Size(1, 1),
      Point(0, 1)
    ) should be(Seq[Point]((0, 1)))

    (Entity.cells(
      Entity.Size(2, 2),
      Point(0, 0)
    ) should contain).theSameElementsAs(
      Seq[Point](
        (0, 0),
        (0, 1),
        (1, 0),
        (1, 1)
      )
    )

    (Entity.cells(
      Entity.Size(2, 2),
      Point(1, 0)
    ) should contain).theSameElementsAs(
      Seq[Point](
        (1, 0),
        (1, 1),
        (2, 0),
        (2, 1)
      )
    )

    (Entity.cells(
      Entity.Size(2, 2),
      Point(0, 1)
    ) should contain).theSameElementsAs(
      Seq[Point](
        (0, 1),
        (0, 2),
        (1, 1),
        (1, 2)
      )
    )

    (Entity.cells(
      Entity.Size(2, 3),
      Point(0, 0)
    ) should contain).theSameElementsAs(
      Seq[Point](
        (0, 0),
        (1, 0),
        (2, 0),
        (0, 1),
        (1, 1),
        (2, 1)
      )
    )

    (Entity.cells(
      Entity.Size(2, 3),
      Point(1, 0)
    ) should contain).theSameElementsAs(
      Seq[Point](
        (1, 0),
        (2, 0),
        (3, 0),
        (1, 1),
        (2, 1),
        (3, 1)
      )
    )

    (Entity.cells(
      Entity.Size(2, 3),
      Point(0, 1)
    ) should contain).theSameElementsAs(
      Seq[Point](
        (0, 1),
        (1, 1),
        (2, 1),
        (0, 2),
        (1, 2),
        (2, 2)
      )
    )

    (Entity.cells(
      Entity.Size(3, 2),
      Point(0, 0)
    ) should contain).theSameElementsAs(
      Seq[Point](
        (0, 0),
        (0, 1),
        (0, 2),
        (1, 0),
        (1, 1),
        (1, 2)
      )
    )

    (Entity.cells(
      Entity.Size(3, 2),
      Point(1, 0)
    ) should contain).theSameElementsAs(
      Seq[Point](
        (1, 0),
        (1, 1),
        (1, 2),
        (2, 0),
        (2, 1),
        (2, 2)
      )
    )

    (Entity.cells(
      Entity.Size(3, 2),
      Point(0, 1)
    ) should contain).theSameElementsAs(
      Seq[Point](
        (0, 1),
        (0, 2),
        (0, 3),
        (1, 1),
        (1, 2),
        (1, 3)
      )
    )
  }

  "Entity Desirability" should "support math ops" in { _ =>
    Desirability(
      self = Cell.Desirability(1),
      r1 = Cell.Desirability(2),
      r2 = Cell.Desirability(3),
      r3 = Cell.Desirability(4),
      r4 = Cell.Desirability(5),
      r5 = Cell.Desirability(6)
    ) + Desirability(
      self = Cell.Desirability(1),
      r1 = Cell.Desirability(2),
      r2 = Cell.Desirability(3),
      r3 = Cell.Desirability(4),
      r4 = Cell.Desirability(5),
      r5 = Cell.Desirability(6)
    ) should be(
      Desirability(
        self = Cell.Desirability(2),
        r1 = Cell.Desirability(4),
        r2 = Cell.Desirability(6),
        r3 = Cell.Desirability(8),
        r4 = Cell.Desirability(10),
        r5 = Cell.Desirability(12)
      )
    )

    Desirability(
      self = Cell.Desirability(1),
      r1 = Cell.Desirability(2),
      r2 = Cell.Desirability(3),
      r3 = Cell.Desirability(4),
      r4 = Cell.Desirability(5),
      r5 = Cell.Desirability(6)
    ) - Desirability(
      self = Cell.Desirability(1),
      r1 = Cell.Desirability(2),
      r2 = Cell.Desirability(3),
      r3 = Cell.Desirability(4),
      r4 = Cell.Desirability(5),
      r5 = Cell.Desirability(6)
    ) should be(
      Desirability(
        self = Cell.Desirability(0),
        r1 = Cell.Desirability(0),
        r2 = Cell.Desirability(0),
        r3 = Cell.Desirability(0),
        r4 = Cell.Desirability(0),
        r5 = Cell.Desirability(0)
      )
    )
  }

  it should "support conversions" in { _ =>
    val desirability = Desirability.fromInt(1, 2, 3, 4, 5, 6)

    desirability should be(
      Desirability(
        self = Cell.Desirability(1),
        r1 = Cell.Desirability(2),
        r2 = Cell.Desirability(3),
        r3 = Cell.Desirability(4),
        r4 = Cell.Desirability(5),
        r5 = Cell.Desirability(6)
      )
    )

    desirability.toMap should be(
      Map(
        0 -> Cell.Desirability(1),
        1 -> Cell.Desirability(2),
        2 -> Cell.Desirability(3),
        3 -> Cell.Desirability(4),
        4 -> Cell.Desirability(5),
        5 -> Cell.Desirability(6)
      )
    )
  }

  it should "have min/max/neutral definitions" in { _ =>
    Desirability.Min should be(
      Desirability(
        self = Cell.Desirability.Min,
        r1 = Cell.Desirability.Min,
        r2 = Cell.Desirability.Min,
        r3 = Cell.Desirability.Min,
        r4 = Cell.Desirability.Min,
        r5 = Cell.Desirability.Min
      )
    )

    Desirability.Max should be(
      Desirability(
        self = Cell.Desirability.Max,
        r1 = Cell.Desirability.Max,
        r2 = Cell.Desirability.Max,
        r3 = Cell.Desirability.Max,
        r4 = Cell.Desirability.Max,
        r5 = Cell.Desirability.Max
      )
    )

    Desirability.Neutral should be(
      Desirability(
        self = Cell.Desirability.Neutral,
        r1 = Cell.Desirability.Neutral,
        r2 = Cell.Desirability.Neutral,
        r3 = Cell.Desirability.Neutral,
        r4 = Cell.Desirability.Neutral,
        r5 = Cell.Desirability.Neutral
      )
    )
  }
}
