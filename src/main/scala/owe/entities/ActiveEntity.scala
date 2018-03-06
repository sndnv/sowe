package owe.entities

import scala.reflect.ClassTag
import akka.actor.{Actor, FSM, Props}
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
  protected def tick(tickSize: Int, properties: P, state: S, modifiers: M): S

  def props(): Props = Props(
    classOf[ActiveEntityActor],
    createProperties(),
    createState(),
    createStateModifiers(),
    createEffects()
  )

  case class Data(state: S, modifiers: M)

  protected class ActiveEntityActor(
    val properties: P,
    val initialState: S,
    val initialModifiers: M,
    val effects: Seq[((P, S) => Boolean, Effect)]
  ) extends Actor
      with FSM[ActiveEntity.State, Data] {
    import ActiveEntity._

    startWith(State.Idle, Data(initialState, initialModifiers))

    whenUnhandled {
      case Event(ApplyEffects(tickSize, externalEffects), data) =>
        val tickModifiers: M = externalEffects.foldLeft(data.modifiers) {
          case (currentModifiers, effect) =>
            effect(tickSize, properties, data.state, currentModifiers) match {
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

        goto(State.Active).using(data.copy(modifiers = tickModifiers))

      case Event(ProcessTick(tickSize), data) =>
        internalBeforeTick(tickSize, data.state, data.modifiers).foreach(msg => context.parent ! msg)
        beforeTick(tickSize, data.state, data.modifiers).foreach(msg => context.parent ! msg)

        val updatedState = tick(tickSize, properties, data.state, data.modifiers)

        afterTick(tickSize, updatedState, data.modifiers).foreach(msg => context.parent ! msg)
        internalAfterTick(tickSize, updatedState, data.modifiers).foreach(msg => context.parent ! msg)

        goto(State.Idle).using(Data(updatedState, initialModifiers))
    }

    onTransition {
      case _ -> State.Idle =>
        //TODO - send updates to parent
        val activeEffects = effects.collect {
          case (p, effect) if p(properties, nextStateData.state) => effect
        }
    }

    initialize()
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

  sealed trait State
  object State {
    case object Idle extends State
    case object Active extends State
  }

  sealed trait Message extends owe.Message

  case class ProcessTick(
    tickSize: Int
  ) extends Message

  case class ApplyEffects[
    P <: Entity.Properties,
    S <: Entity.State,
    M <: Entity.StateModifiers
  ](
    tickSize: Int,
    externalEffects: Seq[Effect[P, S, M]]
  ) extends Message
}
