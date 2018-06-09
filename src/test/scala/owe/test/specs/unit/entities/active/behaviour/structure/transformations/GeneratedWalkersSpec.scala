package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class GeneratedWalkersSpec extends UnitSpec {
  case class FixtureParam() {}

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A GeneratedWalkers transformation" should "create new walkers" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
