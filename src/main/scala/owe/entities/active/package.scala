package owe.entities

package object active {
  final case class AttackRate(value: Int) extends AnyVal
  final case class AttackRateModifier(value: Int) extends AnyVal {
    def apply(rate: AttackRate): AttackRate = AttackRate((rate.value * value) / 100)
  }

  final case class Distance(value: Int) extends AnyVal {
    def +(distance: Distance): Distance = Distance(value + distance.value)
    def -(distance: Distance): Distance = Distance(value - distance.value)
    def >(distance: Distance): Boolean = value > distance.value
    def <(distance: Distance): Boolean = value < distance.value
    def >=(distance: Distance): Boolean = value >= distance.value
    def <=(distance: Distance): Boolean = value <= distance.value
  }

  final case class DistanceModifier(value: Int) extends AnyVal {
    def apply(distance: Distance): Distance = Distance((distance.value * value) / 100)
  }

  final case class AttackDamage(value: Int) extends AnyVal {
    def apply(life: Life): Life = Life((life.value - value).max(0))
  }

  final case class AttackDamageModifier(value: Int) extends AnyVal {
    def apply(damage: AttackDamage): AttackDamage = AttackDamage((damage.value * value) / 100)
  }

  final case class Life(value: Int) extends AnyVal {
    def +(life: Life): Life = Life(value + life.value)
    def -(life: Life): Life = Life(value - life.value)
    def >(life: Life): Boolean = value > life.value
    def <(life: Life): Boolean = value < life.value
    def >=(life: Life): Boolean = value >= life.value
    def <=(life: Life): Boolean = value <= life.value
    def isSufficient: Boolean = value > 0
  }

  final case class Speed(value: Int) extends AnyVal

  final case class SpeedModifier(value: Int) extends AnyVal {
    def apply(speed: Speed): Speed = Speed((speed.value * value) / 100)
  }

  final case class RiskAmount(value: Int) extends AnyVal { // doc - in pct (0% - 100%)
    def +(risk: RiskAmount): RiskAmount = RiskAmount(value + risk.value)
    def >(risk: RiskAmount): Boolean = value > risk.value
    def <(risk: RiskAmount): Boolean = value < risk.value
  }

  object RiskAmount {
    val min: RiskAmount = RiskAmount(0)
    val max: RiskAmount = RiskAmount(100)
  }

  final case class EducationEntry(name: String) extends AnyVal
  final case class EntertainmentEntry(name: String) extends AnyVal
  final case class ReligionEntry(name: String) extends AnyVal
  final case class HealthcareEntry(name: String) extends AnyVal
  final case class CivilServiceEntry(name: String) extends AnyVal

  final case class EducationLevel(current: Int, minimal: Int, required: Int)
  final case class EntertainmentLevel(current: Int, minimal: Int, required: Int)
  final case class ReligionLevel(current: Int, minimal: Int, required: Int)
  final case class HealthcareLevel(current: Int, minimal: Int, required: Int)
  final case class CivilServiceLevel(current: Int, minimal: Int, required: Int)

  final case class EducationLevelModifier(value: Int) extends AnyVal
  final case class EntertainmentLevelModifier(value: Int) extends AnyVal
  final case class ReligionLevelModifier(value: Int) extends AnyVal
  final case class HealthcareLevelModifier(value: Int) extends AnyVal
  final case class CivilServiceLevelModifier(value: Int) extends AnyVal
}
