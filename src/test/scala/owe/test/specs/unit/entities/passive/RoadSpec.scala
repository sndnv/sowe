package owe.test.specs.unit.entities.passive

import org.scalatest.Outcome
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.passive.Road
import owe.test.specs.unit.UnitSpec

class RoadSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Road" should "have correct basic entity properties" in { _ =>
    val entity = new Road
    entity.`size` should be(Entity.Size(height = 1, width = 1))
    entity.`type` should be(Entity.Type.Road)
    entity.`desirability` should be(Desirability.Neutral)
  }
}
