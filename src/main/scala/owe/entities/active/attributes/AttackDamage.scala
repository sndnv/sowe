package owe.entities.active.attributes

final case class AttackDamage(value: Int) extends AnyVal {
  def apply(life: Life): Life = Life((life.value - value).max(0))
}

object AttackDamage {
  final case class Modifier(value: Int) extends AnyVal {
    def apply(damage: AttackDamage): AttackDamage = AttackDamage((damage.value * value) / 100)
  }
}
