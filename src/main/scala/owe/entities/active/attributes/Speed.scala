package owe.entities.active.attributes

final case class Speed(value: Int) extends AnyVal {
  def >(speed: Speed): Boolean = value > speed.value
  def <(speed: Speed): Boolean = value < speed.value
  def >=(speed: Speed): Boolean = value >= speed.value
  def <=(speed: Speed): Boolean = value <= speed.value
}

object Speed {
  final case class Modifier(value: Int) extends AnyVal {
    def apply(speed: Speed): Speed = Speed((speed.value * value) / 100)
  }
}
