package owe.entities

package object active {
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
