package owe.entities.active.attributes

final case class Life(value: Int) extends AnyVal {
  def +(life: Life): Life = Life(value + life.value)
  def -(life: Life): Life = Life(value - life.value)
  def >(life: Life): Boolean = value > life.value
  def <(life: Life): Boolean = value < life.value
  def >=(life: Life): Boolean = value >= life.value
  def <=(life: Life): Boolean = value <= life.value
  def isSufficient: Boolean = value > 0
}
