package owe.entities.active.attributes

final case class Distance(value: Int) extends AnyVal {
  def +(distance: Distance): Distance = Distance(value + distance.value)
  def -(distance: Distance): Distance = Distance(value - distance.value)
  def >(distance: Distance): Boolean = value > distance.value
  def <(distance: Distance): Boolean = value < distance.value
  def >=(distance: Distance): Boolean = value >= distance.value
  def <=(distance: Distance): Boolean = value <= distance.value
}

object Distance {
  final case class Modifier(value: Int) extends AnyVal {
    def apply(distance: Distance): Distance = Distance((distance.value * value) / 100)
  }
}
