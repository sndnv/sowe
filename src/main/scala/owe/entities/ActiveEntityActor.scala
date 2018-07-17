package owe.entities

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, MapData}
import owe.map.GameMap
import owe.map.GameMap.EntityTickProcessed

import scala.reflect.ClassTag

class ActiveEntityActor[
  P <: Entity.Properties: ClassTag,
  S <: Entity.State: ClassTag,
  M <: Entity.StateModifiers: ClassTag
](
  val initialEntityDataFn: ActiveEntityRef => Data,
  val effects: Seq[(Data => Boolean, Effect)],
  val actorToActiveEntityRef: ActorRef => ActiveEntityRef,
  val behaviourProps: () => Props
)(implicit timeout: Timeout)
    extends Actor
    with Stash
    with ActorLogging {

  import ActiveEntityActor._
  import context.dispatcher

  private val selfRef: ActiveEntityRef = actorToActiveEntityRef(self)

  private val initialEntityData: Data = initialEntityDataFn(selfRef)

  private val behaviourHandler: ActorRef = {
    context.actorOf(
      behaviourProps(),
      s"Behaviour"
    )
  }

  private val parentMap: ActorRef = context.parent

  def active(processingData: ProcessingData): Receive = {
    case BehaviourTickProcessed(tick, updatedState) =>
      log.debug("State updated to [{}]; tick [{}] complete", tick, updatedState)
      unstashAll()
      parentMap ! EntityTickProcessed(tick)
      context.become(idle(ProcessingData(initialEntityData.withState(updatedState), Seq.empty)))

    case ForwardMessage(message) =>
      log.debug("Forwarding message [{}] to parent map while active", message)
      (parentMap ? message).pipeTo(sender)

    case GetData() =>
      log.debug("Responding to [{}] with entity data while active", sender)
      sender ! processingData.entityData

    case message =>
      log.debug("Stashing message [{}] from sender [{}]", message, sender)
      stash()
  }

  def idle(processingData: ProcessingData): Receive = {
    case ApplyEffects(externalEffects) =>
      log.debug("Applying [{}] effects: [{}]", externalEffects.size, externalEffects)
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

    case ProcessEntityTick(tick, map) =>
      log.debug("Starting tick [{}] processing with map data: [{}]", tick, map)
      behaviourHandler ! ProcessBehaviourTick(tick, map, processingData.entityData, processingData.messages)
      context.become(active(processingData))

    case GetActiveEffects() =>
      log.debug("Responding to sender [{}] with active effects", sender)
      val activeEffects = effects.collect {
        case (p, effect) if p(processingData.entityData) => effect
      }
      sender ! activeEffects

    case GetData() =>
      log.debug("Responding to [{}] with entity data while idle", sender)
      sender ! processingData.entityData

    case ForwardMessage(message) =>
      log.debug("Forwarding message [{}] to parent map while idle", message)
      (parentMap ? message).pipeTo(sender)

    case AddEntityMessage(message) =>
      log.debug("Received entity message [{}]", message)
      context.become(idle(processingData.copy(messages = processingData.messages :+ message)))
  }

  override def receive: Receive = idle(ProcessingData(initialEntityData, Seq.empty))
}

object ActiveEntityActor {

  private case class ProcessingData(entityData: Data, messages: Seq[Entity.Message])

  sealed trait ProcessingMessage extends owe.Message
  case class AddEntityMessage(message: Entity.Message) extends ProcessingMessage
  case class ForwardMessage(message: GameMap.Message) extends ProcessingMessage
  case class GetActiveEffects() extends ProcessingMessage
  case class GetData() extends ProcessingMessage
  case class ApplyEffects(externalEffects: Seq[owe.effects.Effect]) extends ProcessingMessage
  case class ProcessEntityTick(tick: Int, map: MapData) extends ProcessingMessage
  case class ProcessBehaviourTick(
    tick: Int,
    map: MapData,
    entity: Data,
    messages: Seq[Entity.Message]
  ) extends ProcessingMessage
  case class BehaviourTickProcessed[S <: Entity.State: ClassTag](tick: Int, state: S)
}
