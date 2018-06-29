package owe.entities

import akka.pattern.{ask, pipe}
import akka.util.Timeout
import owe.Tagging._
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityActorRef, ActiveEntityData, EntityMessage}
import owe.entities.active.behaviour.BaseBehaviour
import owe.entities.active.{Resource, Structure, Walker}
import owe.map.grid.Point
import owe.map.{Cell, GameMap}
import scala.reflect.ClassTag

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, StashBuffer}
import owe.entities.Entity.EntityActorRef
import owe.entities.active.Resource.ResourceActorRef
import owe.entities.active.Structure.StructureActorRef
import owe.entities.active.Walker.WalkerActorRef

abstract class ActiveEntity[
  P <: Entity.Properties: ClassTag,
  S <: Entity.State: ClassTag,
  M <: Entity.StateModifiers: ClassTag,
  B <: BaseBehaviour: ClassTag
]() extends Entity {
  import ActiveEntity._

  protected def createActiveEntityData(): ActiveEntityActorRef => ActiveEntityData

  protected def createEffects(): Seq[(ActiveEntityData => Boolean, owe.effects.Effect)]

  protected def createBehaviour(): B

  def setup(
    parentMap: ActorRef[GameMap.Message]
  ): Behavior[EntityMessage] =
    ActiveEntityActor.setup(
      createActiveEntityData(),
      createEffects(),
      parentMap
    )

  case class ProcessingData(entityData: ActiveEntityData, messages: Seq[Entity.Message])

  private object ActiveEntityActor {

    def setup(
      initialEntityDataFn: ActiveEntityActorRef => ActiveEntityData,
      effects: Seq[(ActiveEntityData => Boolean, owe.effects.Effect)],
      parentMap: ActorRef[GameMap.Message]
    ): Behavior[EntityMessage] = Behaviors.setup[EntityMessage] { ctx =>
      import ctx.executionContext

      val buffer = StashBuffer[EntityMessage](capacity = 1000) // TODO - capacity

      val definitions = ActiveEntityDefinitions(
        behaviourHandler = ctx.spawn(
          behavior = createBehaviour().setup(),
          name = s"""${ctx.self.path.name}-behaviour"""
        ),
        initialEntityData = initialEntityDataFn(ctx.self),
        parentMap,
        effects,
        buffer
      )

      idle(ProcessingData(definitions.initialEntityData, Seq.empty), definitions)
    }

    def active(
      processingData: ProcessingData,
      definitions: ActiveEntityDefinitions
    ): Behavior[EntityMessage] = Behaviors.receive { (ctx, msg) =>
      msg match {
        case UpdateState(updatedState) =>
          definitions.buffer.unstashAll(
            ctx,
            idle(ProcessingData(definitions.initialEntityData.withState(updatedState), Seq.empty), definitions)
          )

        case ForwardMessage(message) =>
          (definitions.parentMap ? message).pipeTo(sender)
          Behaviors.same

        case GetData() =>
          sender ! processingData.entityData
          Behaviors.same

        case _ =>
          definitions.buffer.stash(msg)
          Behaviors.same
      }
    }

    def idle(
      processingData: ProcessingData,
      definitions: ActiveEntityDefinitions
    ): Behavior[EntityMessage] = Behaviors.receive { (ctx, msg) =>
      msg match {
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
                      ctx.log.error(
                        "Unexpected modifiers type [{}] returned by effect [{}]",
                        unexpectedModifiers.getClass.getName,
                        effect.getClass.getName
                      )
                      currentModifiers
                  }

                case unusableEffect =>
                  ctx.log.debug(
                    "Unusable effect received: [{}]",
                    unusableEffect.getClass.getName,
                    effect.getClass.getName
                  )
                  currentModifiers
              }
          }

          idle(
            processingData.copy(entityData = processingData.entityData.withModifiers(tickModifiers)),
            definitions
          )

        case ProcessGameTick(map) =>
          definitions.behaviourHandler ! ProcessEntityTick(map, processingData.entityData, processingData.messages)
          active(processingData, definitions)

        case GetActiveEffects() =>
          val activeEffects = definitions.effects.collect {
            case (p, effect) if p(processingData.entityData) => effect
          }
          sender ! activeEffects
          Behaviors.same

        case ForwardMessage(message) =>
          (definitions.parentMap ? message).pipeTo(sender)
          Behaviors.same

        case AddEntityMessage(message) =>
          idle(processingData.copy(messages = processingData.messages :+ message), definitions)
      }
    }
  }
}

object ActiveEntity {
  trait ActiveEntityActorRef extends EntityActorRef

  trait Effect[
    P <: Entity.Properties,
    S <: Entity.State,
    M <: Entity.StateModifiers
  ] extends owe.effects.Effect {
    def apply(entityData: ActiveEntityData): M
  }

  case class MapData(position: Point, cellState: Cell.State)

  // TODO - name
  private case class ActiveEntityDefinitions(
    behaviourHandler: ActorRef[EntityBehaviourMessage],
    initialEntityData: ActiveEntityData,
    parentMap: ActorRef[GameMap.Message],
    effects: Seq[(ActiveEntityData => Boolean, owe.effects.Effect)],
    buffer: StashBuffer[EntityMessage]
  )

  sealed trait ActiveEntityData {
    def properties: Entity.Properties
    def state: Entity.State
    def modifiers: Entity.StateModifiers
    def id: ActiveEntityActorRef
    def withState(newState: Entity.State): ActiveEntityData
    def withModifiers(newModifiers: Entity.StateModifiers): ActiveEntityData
  }

  case class ResourceData(
    properties: Resource.Properties,
    state: Resource.State,
    modifiers: Resource.StateModifiers,
    id: ResourceActorRef
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
    id: StructureActorRef
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
    id: WalkerActorRef
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

  sealed trait EntityMessage extends owe.Message

  case class AddEntityMessage(message: Entity.Message) extends EntityMessage

  case class ForwardMessage(message: GameMap.Message) extends EntityMessage

  case class GetActiveEffects() extends EntityMessage

  case class GetData() extends EntityMessage

  case class ApplyEffects(externalEffects: Seq[owe.effects.Effect]) extends EntityMessage

  case class ProcessGameTick(map: MapData) extends EntityMessage

  case class UpdateState[S <: Entity.State](updatedState: S) extends EntityMessage

  // TODO - move ?
  sealed trait EntityBehaviourMessage extends owe.Message

  case class ProcessEntityTick(map: MapData, entity: ActiveEntityData, messages: Seq[Entity.Message])
      extends EntityBehaviourMessage

  private[entities] trait Become extends EntityBehaviourMessage {
    def behaviour: () => Behavior[EntityBehaviourMessage]
    def data: ActiveEntityData
  }

}
