package owe.test.specs.unit.entities.active.attributes

import org.scalatest.Outcome
import owe.entities.active.attributes.{AttackDamage, AttackRate}
import owe.test.specs.unit.UnitSpec

class AttackRateSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "Attack Rate" should "be applicable to Attack Damage instances" in { _ =>
    AttackRate(200)(AttackDamage(35)) should be(AttackDamage(70))
    AttackRate(200)(AttackDamage(1)) should be(AttackDamage(2))
    AttackRate(10)(AttackDamage(30)) should be(AttackDamage(3))
  }

  "Attack Rate Modifier" should "be applied to Attack Rate" in { _ =>
    AttackRate.Modifier(50)(AttackRate(30)) should be(AttackRate(15))
  }
}
