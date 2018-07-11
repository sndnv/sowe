package owe.test.specs.unit.entities.active.attributes

import org.scalatest.Outcome
import owe.entities.active.attributes.Speed
import owe.test.specs.unit.UnitSpec

class SpeedSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Risk Amount" should "support comparison ops" in { _ =>
    Speed(50) > Speed(10) should be(true)
    Speed(50) < Speed(10) should be(false)
    Speed(50) >= Speed(10) should be(true)
    Speed(50) >= Speed(50) should be(true)
    Speed(50) <= Speed(10) should be(false)
    Speed(50) <= Speed(50) should be(true)
    Speed(50) == Speed(10) should be(false)
    Speed(50) == Speed(50) should be(true)
  }

  "Speed Modifier" should "be applied to Speed" in { _ =>
    Speed.Modifier(50)(Speed(30)) should be(Speed(15))
  }
}
