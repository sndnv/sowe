package owe.entities.active.attributes

final case class RiskAmount(value: Int) extends AnyVal { // doc - in pct (0% - 100%)
  def +(risk: RiskAmount): RiskAmount = RiskAmount(value + risk.value).min(RiskAmount.max).max(RiskAmount.min)
  def >(risk: RiskAmount): Boolean = value > risk.value
  def <(risk: RiskAmount): Boolean = value < risk.value
  def >=(risk: RiskAmount): Boolean = value >= risk.value
  def <=(risk: RiskAmount): Boolean = value <= risk.value
  def max(risk: RiskAmount): RiskAmount = RiskAmount(math.max(value, risk.value))
  def min(risk: RiskAmount): RiskAmount = RiskAmount(math.min(value, risk.value))
}

object RiskAmount {
  val min: RiskAmount = RiskAmount(0)
  val max: RiskAmount = RiskAmount(100)
}
