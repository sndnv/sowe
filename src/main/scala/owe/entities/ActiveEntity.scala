package owe.entities

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, ActiveEntityRef}
import owe.entities.Entity.EntityRef
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.behaviour.BaseBehaviour
import owe.entities.active.{Resource, Structure, Walker}
import owe.map.grid.Point
import owe.map.{Cell, GameMap}

import scala.reflect.ClassTag

abstract class ActiveEntity[
  P <: Entity.Properties: ClassTag,
  S <: Entity.State: ClassTag,
  M <: Entity.StateModifiers: ClassTag,
  B <: BaseBehaviour: ClassTag
]() extends Entity {
  protected def createActiveEntityData(): ActiveEntityRef => ActiveEntityData

  protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)]

  protected def createBehaviour(): B

  protected def actorToActiveEntityRef(ref: ActorRef): ActiveEntityRef

  override def props()(implicit timeout: Timeout): Props = Props(
    classOf[ActiveEntityActor],
    createActiveEntityData(),
    createEffects(),
    timeout
  )

  private def behaviourProps(): Props = Props(createBehaviour())

  case class ProcessingData(entityData: ActiveEntityData, messages: Seq[Entity.Message])

  private class ActiveEntityActor(
    val initialEntityDataFn: ActiveEntityRef => ActiveEntityData,
    val effects: Seq[(ActiveEntityData => Boolean, Effect)]
  )(implicit timeout: Timeout)
      extends Actor
      with Stash
      with ActorLogging {

    import ActiveEntity._
    import context.dispatcher

    private val selfRef: ActiveEntityRef = actorToActiveEntityRef(self)

    private val initialEntityData: ActiveEntityData = initialEntityDataFn(selfRef)

    private val behaviourHandler: ActorRef = context.actorOf(
      behaviourProps(),
      s"""${self.path.name}-behaviour"""
    )

    private val parentMap: ActorRef = context.parent

    def active(processingData: ProcessingData): Receive = {
      case updatedState: S =>
        unstashAll()
        context.become(idle(ProcessingData(initialEntityData.withState(updatedState), Seq.empty)))

      case ForwardMessage(message) =>
        (parentMap ? message).pipeTo(sender)

      case GetData() =>
        sender ! processingData.entityData

      case _ =>
        stash()
    }

    def idle(processingData: ProcessingData): Receive = {
      case ApplyEffects(externalEffects) =>
        val tickModifiers: Entity.StateModifiers = externalEffects.foldLeft(processingData.entityData.modifiers) {
          case (currentModifiers, effect) =>
            effect match {
              case effect: ActiveEntity.Effect[P, S, M] =>
                effect(
                  processingData.entityData.withModifiers(currentModifiers)
                ) match {
                  case updatedModifiers: M =>
                    updatedModifiers

                  case unexpectedModifiers =>
                    log.error(
                      "Unexpected modifiers type [{}] returned by effect [{}]",
                      unexpectedModifiers.getClass.getName,
                      effect.getClass.getName
                    )
                    currentModifiers
                }

              case unusableEffect =>
                log.debug(
                  "Unusable effect received: [{}]",
                  unusableEffect.getClass.getName,
                  effect.getClass.getName
                )
                currentModifiers
            }
        }

        context.become(
          idle(
            processingData.copy(entityData = processingData.entityData.withModifiers(tickModifiers))
          )
        )

      case ProcessGameTick(map) =>
        behaviourHandler ! ProcessEntityTick(map, processingData.entityData, processingData.messages)
        context.become(active(processingData))

      case GetActiveEffects() =>
        val activeEffects = effects.collect {
          case (p, effect) if p(processingData.entityData) => effect
        }
        sender ! activeEffects

      case ForwardMessage(message) =>
        (parentMap ? message).pipeTo(sender)

      case AddEntityMessage(message) =>
        context.become(idle(processingData.copy(messages = processingData.messages :+ message)))
    }

    override def receive: Receive = idle(ProcessingData(initialEntityData, Seq.empty))
  }
}

object ActiveEntity {
  trait ActiveEntityRef extends EntityRef

  trait Effect[
    P <: Entity.Properties,
    S <: Entity.State,
    M <: Entity.StateModifiers
  ] extends owe.effects.Effect {
    def apply(entityData: ActiveEntityData): M
  }

  case class MapData(position: Point, cellState: Cell.State)

  sealed trait ActiveEntityData {
    def properties: Entity.Properties
    def state: Entity.State
    def modifiers: Entity.StateModifiers
    def id: ActiveEntityRef
    def withState(newState: Entity.State): ActiveEntityData
    def withModifiers(newModifiers: Entity.StateModifiers): ActiveEntityData
  }

  case class ResourceData(
    properties: Resource.Properties,
    state: Resource.State,
    modifiers: Resource.StateModifiers,
    id: ResourceRef
  ) extends ActiveEntityData {
    override def withState(newState: Entity.State): ActiveEntityData =
      newState match {
        case resourceState: Resource.State => copy(state = resourceState)
      }

    override def withModifiers(newModifiers: Entity.StateModifiers): ActiveEntityData =
      newModifiers match {
        case resourceModifiers: Resource.StateModifiers => copy(modifiers = resourceModifiers)
      }
  }

  case class StructureData(
    properties: Structure.Properties,
    state: Structure.State,
    modifiers: Structure.StateModifiers,
    id: StructureRef
  ) extends ActiveEntityData {
    override def withState(newState: Entity.State): ActiveEntityData =
      newState match {
        case structureState: Structure.State => copy(state = structureState)
      }

    override def withModifiers(newModifiers: Entity.StateModifiers): ActiveEntityData =
      newModifiers match {
        case structureModifiers: Structure.StateModifiers => copy(modifiers = structureModifiers)
      }
  }

  case class WalkerData(
    properties: Walker.Properties,
    state: Walker.State,
    modifiers: Walker.StateModifiers,
    id: WalkerRef
  ) extends ActiveEntityData {
    override def withState(newState: Entity.State): ActiveEntityData =
      newState match {
        case walkerState: Walker.State => copy(state = walkerState)
      }

    override def withModifiers(newModifiers: Entity.StateModifiers): ActiveEntityData =
      newModifiers match {
        case walkerModifiers: Walker.StateModifiers => copy(modifiers = walkerModifiers)
      }
  }

  sealed trait ProcessingMessage extends owe.Message
  case class AddEntityMessage(message: Entity.Message) extends ProcessingMessage
  case class ForwardMessage(message: GameMap.Message) extends ProcessingMessage
  case class GetActiveEffects() extends ProcessingMessage
  case class GetData() extends ProcessingMessage
  case class ApplyEffects(externalEffects: Seq[owe.effects.Effect]) extends ProcessingMessage
  case class ProcessGameTick(map: MapData) extends ProcessingMessage
  case class ProcessEntityTick(map: MapData, entity: ActiveEntityData, messages: Seq[Entity.Message])
      extends ProcessingMessage
}
