package owe.entities

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data}
import owe.entities.Entity.EntityRef
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.behaviour.BaseBehaviour
import owe.entities.active.{Resource, Structure, Walker}
import owe.map.Cell
import owe.map.grid.Point

import scala.reflect.ClassTag

abstract class ActiveEntity[
  P <: Entity.Properties: ClassTag,
  S <: Entity.State: ClassTag,
  M <: Entity.StateModifiers: ClassTag,
  B <: BaseBehaviour: ClassTag
]() extends Entity {
  protected def createActiveEntityData(): ActiveEntityRef => Data

  protected def createEffects(): Seq[(Data => Boolean, Effect)]

  protected def createBehaviour(): B

  protected def actorToActiveEntityRef(ref: ActorRef): ActiveEntityRef

  final override def props()(implicit timeout: Timeout): Props = Props(
    new ActiveEntityActor[P, S, M](
      createActiveEntityData(),
      createEffects(),
      (ref: ActorRef) => actorToActiveEntityRef(ref),
      () => behaviourProps()
    )
  )

  private def behaviourProps(): Props = Props(createBehaviour())
}

object ActiveEntity {
  trait Instruction extends owe.Instruction

  trait ActiveEntityRef extends EntityRef

  trait Effect[
    P <: Entity.Properties,
    S <: Entity.State,
    M <: Entity.StateModifiers
  ] extends owe.effects.Effect {
    def apply(entityData: Data): M
  }

  case class MapData(position: Point, cellState: Cell.State)

  sealed trait Data {
    def properties: Entity.Properties
    def state: Entity.State
    def modifiers: Entity.StateModifiers
    def id: ActiveEntityRef
    def withState(newState: Entity.State): Data
    def withModifiers(newModifiers: Entity.StateModifiers): Data
  }

  case class ResourceData(
    properties: Resource.Properties,
    state: Resource.State,
    modifiers: Resource.StateModifiers,
    id: ResourceRef
  ) extends Data {
    override def withState(newState: Entity.State): Data =
      newState match {
        case resourceState: Resource.State => copy(state = resourceState)
      }

    override def withModifiers(newModifiers: Entity.StateModifiers): Data =
      newModifiers match {
        case resourceModifiers: Resource.StateModifiers => copy(modifiers = resourceModifiers)
      }
  }

  case class StructureData(
    properties: Structure.Properties,
    state: Structure.State,
    modifiers: Structure.StateModifiers,
    id: StructureRef
  ) extends Data {
    override def withState(newState: Entity.State): Data =
      newState match {
        case structureState: Structure.State => copy(state = structureState)
      }

    override def withModifiers(newModifiers: Entity.StateModifiers): Data =
      newModifiers match {
        case structureModifiers: Structure.StateModifiers => copy(modifiers = structureModifiers)
      }
  }

  case class WalkerData(
    properties: Walker.Properties,
    state: Walker.State,
    modifiers: Walker.StateModifiers,
    id: WalkerRef
  ) extends Data {
    override def withState(newState: Entity.State): Data =
      newState match {
        case walkerState: Walker.State => copy(state = walkerState)
      }

    override def withModifiers(newModifiers: Entity.StateModifiers): Data =
      newModifiers match {
        case walkerModifiers: Walker.StateModifiers => copy(modifiers = walkerModifiers)
      }
  }

}
