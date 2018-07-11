package owe.entities.active.attributes

final case class AttackRate(value: Int) extends AnyVal {
  def apply(damage: AttackDamage): AttackDamage = AttackDamage((damage.value * value) / 100)
}

object AttackRate {
  final case class Modifier(value: Int) extends AnyVal {
    def apply(rate: AttackRate): AttackRate = AttackRate((rate.value * value) / 100)
  }
}
