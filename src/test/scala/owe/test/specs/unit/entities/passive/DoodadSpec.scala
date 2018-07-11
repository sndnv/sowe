package owe.test.specs.unit.entities.passive

import org.scalatest.Outcome
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.passive.Doodad
import owe.test.specs.unit.UnitSpec

class DoodadSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Doodad" should "have correct basic entity properties" in { _ =>
    val entity = new Doodad
    entity.`size` should be(Entity.Size(height = 1, width = 1))
    entity.`type` should be(Entity.Type.Doodad)
    entity.`desirability` should be(Desirability.Neutral)
  }
}
