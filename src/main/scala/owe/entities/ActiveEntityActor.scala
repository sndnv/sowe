package owe.entities

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, Instruction, MapData}
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

  def active(tick: Int, processingData: ProcessingData): Receive = {
    case BehaviourTickProcessed(updatedState) =>
      log.debug("State updated to [{}]; tick [{}] complete", updatedState, tick)
      unstashAll()
      parentMap ! EntityTickProcessed(tick)
      context.become(idle(processingData.copy(entityData = initialEntityData.withState(updatedState))))

    case ForwardMessage(message) =>
      log.debug("Forwarding message [{}] to parent map while active", message)
      val senderRef = sender
      (parentMap ? message).foreach { response =>
        if (senderRef != context.system.deadLetters) {
          senderRef ! response
        }
      }

    case GetData() =>
      log.debug("Responding to [{}] with entity data while active", sender)
      sender ! processingData.entityData

    case message =>
      log.debug(
        "Stashing message [{}] from sender [{}] while active for tick [{}]",
        message,
        sender,
        tick
      )
      stash()
  }

  def processingMessages(tick: Int, map: MapData, processingData: ProcessingData): Receive = {
    case InstructionsApplied() =>
      log.debug("Instructions for tick [{}] applied", tick)

    case MessagesApplied(updatedState) =>
      log.debug("Messages for tick [{}] applied", tick)
      val updatedData = processingData.copy(entityData = processingData.entityData.withState(updatedState))
      behaviourHandler ! ProcessBehaviourTick(map, updatedData.entityData)
      unstashAll()
      context.become(active(tick, updatedData))

    case message =>
      log.debug(
        "Stashing message [{}] from sender [{}] while processing messages for tick [{}]",
        message,
        sender,
        tick
      )
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
              log.error(
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
      if (processingData.instructions.nonEmpty) {
        behaviourHandler ! ApplyInstructions(processingData.entityData, processingData.instructions)
      }

      behaviourHandler ! ApplyMessages(processingData.entityData, processingData.messages)
      context.become(processingMessages(tick, map, processingData.copy(messages = Seq.empty, instructions = Seq.empty)))

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

    case AddEntityInstruction(instruction) =>
      log.debug("Received entity instruction [{}]", instruction)
      context.become(idle(processingData.copy(instructions = processingData.instructions :+ instruction)))
  }

  override def receive: Receive = idle(ProcessingData(initialEntityData, Seq.empty, Seq.empty))
}

object ActiveEntityActor {

  private case class ProcessingData(
    entityData: Data,
    messages: Seq[Entity.Message],
    instructions: Seq[Instruction]
  )

  sealed trait ProcessingMessage extends owe.Message
  private[owe] case class ApplyInstructions(entity: Data, instructions: Seq[Instruction]) extends ProcessingMessage
  private[owe] case class InstructionsApplied() extends ProcessingMessage
  private[owe] case class ApplyMessages(entity: Data, messages: Seq[Entity.Message]) extends ProcessingMessage
  private[owe] case class MessagesApplied[S <: Entity.State: ClassTag](state: S) extends ProcessingMessage
  case class AddEntityInstruction(instruction: Instruction) extends ProcessingMessage
  case class AddEntityMessage(message: Entity.Message) extends ProcessingMessage
  case class ForwardMessage(message: GameMap.Message) extends ProcessingMessage
  case class GetActiveEffects() extends ProcessingMessage
  case class GetData() extends ProcessingMessage
  case class ApplyEffects(externalEffects: Seq[owe.effects.Effect]) extends ProcessingMessage
  case class ProcessEntityTick(tick: Int, map: MapData) extends ProcessingMessage
  case class ProcessBehaviourTick(map: MapData, entity: Data) extends ProcessingMessage
  case class BehaviourTickProcessed[S <: Entity.State: ClassTag](state: S)
}
