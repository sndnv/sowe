package owe.entities

import scala.reflect.ClassTag
import akka.actor.{Actor, ActorLogging, Props}
import owe.effects.Effect

abstract class ActiveEntity[
  P <: Entity.Properties,
  S <: Entity.State,
  M <: Entity.StateModifiers: ClassTag,
  T <: ActiveEntity.ActorRefTag
]() extends Entity {
  type Tag = T

  protected def beforeTick(tickSize: Int, state: S, modifiers: M): Seq[owe.Message] = Seq.empty
  protected def afterTick(tickSize: Int, state: S, modifiers: M): Seq[owe.Message] = Seq.empty
  //TODO - correct access modifiers?
  protected[entities] def internalBeforeTick(tickSize: Int, state: S, modifiers: M): Seq[owe.Message] = Seq.empty
  protected[entities] def internalAfterTick(tickSize: Int, state: S, modifiers: M): Seq[owe.Message] = Seq.empty
  protected def createProperties(): P
  protected def createState(): S
  protected def createStateModifiers(): M
  protected def createEffects(): Seq[((P, S) => Boolean, Effect)]
  protected def tick(tickSize: Int, state: S, modifiers: M): S

  def props(): Props = Props(
    classOf[ActiveEntityActor],
    createProperties(),
    createState(),
    createStateModifiers(),
    createEffects()
  )

  protected class ActiveEntityActor(
    val properties: P,
    var state: S,
    val modifiers: M,
    var effects: Seq[((P, S) => Boolean, Effect)]
  ) extends Actor
      with ActorLogging {
    override def receive: Receive = {
      case message: ActiveEntity.ProcessTick[P, S, M] =>
        internalBeforeTick(message.tickSize, state, modifiers).foreach(msg => context.parent ! msg)
        beforeTick(message.tickSize, state, modifiers).foreach(msg => context.parent ! msg)

        val tickModifiers: M = message.externalEffects.foldLeft(modifiers) {
          case (currentModifiers, effect) =>
            effect(message.tickSize, properties, state, currentModifiers) match {
              case updatedModifiers: M =>
                updatedModifiers

              case unexpectedModifiers =>
                //TODO - should be unreachable
                log.debug(
                  "Unexpected modifiers type [{}] returned by effect [{}]",
                  unexpectedModifiers.getClass.getName,
                  effect.getClass.getName
                )
                currentModifiers
            }
        }

        state = tick(message.tickSize, state, tickModifiers)

        afterTick(message.tickSize, state, tickModifiers).foreach(msg => context.parent ! msg)
        internalAfterTick(message.tickSize, state, tickModifiers).foreach(msg => context.parent ! msg)

        //TODO - send updates to parent
        val activeEffects = effects.collect {
          case (p, effect) if p(properties, state) => effect
        }
    }
  }
}

object ActiveEntity {
  trait ActorRefTag

  trait Effect[
    P <: Entity.Properties,
    S <: Entity.State,
    M <: Entity.StateModifiers
  ] extends owe.effects.Effect {
    def apply(tickSize: Int, properties: P, state: S, modifiers: M): M
  }

  sealed trait Message extends owe.Message

  case class ProcessTick[
    P <: Entity.Properties,
    S <: Entity.State,
    M <: Entity.StateModifiers
  ](
    tickSize: Int,
    externalEffects: Seq[Effect[P, S, M]]
  ) extends Message
}
