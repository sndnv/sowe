package owe.test.specs.unit.entities.active.attributes

import org.scalatest.Outcome
import owe.entities.active.attributes.Distance
import owe.test.specs.unit.UnitSpec

class DistanceSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Distance" should "support math ops" in { _ =>
    Distance(50) + Distance(60) should be(Distance(110))
    Distance(50) - Distance(60) should be(Distance(-10))
  }

  it should "support comparison ops" in { _ =>
    Distance(50) > Distance(10) should be(true)
    Distance(50) < Distance(10) should be(false)
    Distance(50) >= Distance(10) should be(true)
    Distance(50) >= Distance(50) should be(true)
    Distance(50) <= Distance(10) should be(false)
    Distance(50) <= Distance(50) should be(true)
    Distance(50) == Distance(10) should be(false)
    Distance(50) == Distance(50) should be(true)
  }

  "Distance Modifier" should "be applied to Distance" in { _ =>
    Distance.Modifier(50)(Distance(30)) should be(Distance(15))
  }
}
