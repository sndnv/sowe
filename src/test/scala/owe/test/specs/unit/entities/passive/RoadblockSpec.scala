package owe.test.specs.unit.entities.passive

import org.scalatest.Outcome
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.passive.Roadblock
import owe.test.specs.unit.UnitSpec

class RoadblockSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Roadblock" should "have correct basic entity properties" in { _ =>
    val entity = new Roadblock
    entity.`size` should be(Entity.Size(height = 1, width = 1))
    entity.`type` should be(Entity.Type.Roadblock)
    entity.`desirability` should be(Desirability.Neutral)
  }
}
