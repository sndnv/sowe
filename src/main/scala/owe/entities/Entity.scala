package owe.entities

trait Entity {
  def `size`: Entity.Size
  def `type`: Entity.Type
}

object Entity {
  trait Properties

  trait State

  trait StateModifiers

  case class Size(height: Int, width: Int)

  sealed trait Type
  object Type {
    case object Road extends Type
    case object Roadblock extends Type
    case object Doodad extends Type
    case object Resource extends Type
    case object Structure extends Type
    case object Walker extends Type
  }
}
