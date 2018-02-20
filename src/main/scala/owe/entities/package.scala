package owe

package object entities {

  trait EntityProperties

  trait EntityState

  trait EntityStateModifiers

  case class EntitySize(height: Int, width: Int) //TODO

  sealed trait EntityType
  object EntityType {
    case object Road extends EntityType
    case object Roadblock extends EntityType
    case object Doodad extends EntityType
    case object Resource extends EntityType
    case object Structure extends EntityType
    case object Walker extends EntityType
  }
}
