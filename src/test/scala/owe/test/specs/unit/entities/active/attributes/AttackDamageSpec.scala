package owe.test.specs.unit.entities.active.attributes

import org.scalatest.Outcome
import owe.entities.active.attributes.{AttackDamage, Life}
import owe.test.specs.unit.UnitSpec

class AttackDamageSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Attack Damage" should "be applicable to Life instances" in { _ =>
    AttackDamage(30)(Life(50)) should be(Life(20))
    AttackDamage(30)(Life(40)) should be(Life(10))
    AttackDamage(30)(Life(30)) should be(Life(0))
    AttackDamage(30)(Life(20)) should be(Life(0))
    AttackDamage(30)(Life(10)) should be(Life(0))
    AttackDamage(30)(Life(0)) should be(Life(0))
  }

  "Attack Damage Modifier" should "be applied to Attack Damage" in { _ =>
    AttackDamage.Modifier(50)(AttackDamage(30)) should be(AttackDamage(15))
  }
}
