package owe.test.specs.unit.entities.active.attributes

import org.scalatest.Outcome
import owe.entities.active.attributes.Life
import owe.test.specs.unit.UnitSpec

class LifeSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Life" should "support math ops" in { _ =>
    Life(50) + Life(60) should be(Life(110))
    Life(50) - Life(60) should be(Life(-10))
  }

  it should "support comparison ops" in { _ =>
    Life(50) > Life(10) should be(true)
    Life(50) < Life(10) should be(false)
    Life(50) >= Life(10) should be(true)
    Life(50) >= Life(50) should be(true)
    Life(50) <= Life(10) should be(false)
    Life(50) <= Life(50) should be(true)
    Life(50) == Life(10) should be(false)
    Life(50) == Life(50) should be(true)
    Life(50).isSufficient should be(true)
    Life(1).isSufficient should be(true)
    Life(0).isSufficient should be(false)
  }
}
