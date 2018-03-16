package owe.entities

import akka.actor.{Actor, FSM, Props}
import owe.effects.Effect
import owe.map.MapCell

import scala.reflect.ClassTag

abstract class ActiveEntity[
  P <: Entity.Properties,
  S <: Entity.State,
  M <: Entity.StateModifiers: ClassTag,
  T <: ActiveEntity.ActorRefTag
]() extends Entity {
  type Tag = T

  protected def beforeTick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: S,
    modifiers: M
  ): Seq[owe.Message] = Seq.empty

  protected def afterTick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: S,
    modifiers: M
  ): Seq[owe.Message] = Seq.empty

  private[entities] def internalBeforeTick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: S,
    modifiers: M
  ): Seq[owe.Message] = Seq.empty

  private[entities] def internalAfterTick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: S,
    modifiers: M
  ): Seq[owe.Message] = Seq.empty

  protected def tick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    properties: P,
    state: S,
    modifiers: M
  ): S

  protected def createProperties(): P

  protected def createState(): S

  protected def createStateModifiers(): M

  protected def createEffects(): Seq[((P, S) => Boolean, Effect)]

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
            effect match {
              case effect: ActiveEntity.Effect[P, S, M] =>
                effect(tickSize, properties, data.state, currentModifiers) match {
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

        goto(State.Active).using(data.copy(modifiers = tickModifiers))

      case Event(ProcessTick(tickSize, cellProperties, cellModifiers), data) =>
        internalBeforeTick(tickSize, cellProperties, cellModifiers, data.state, data.modifiers).foreach { msg =>
          context.parent ! msg
        }

        beforeTick(tickSize, cellProperties, cellModifiers, data.state, data.modifiers).foreach { msg =>
          context.parent ! msg
        }

        val updatedState = tick(tickSize, cellProperties, cellModifiers, properties, data.state, data.modifiers)

        afterTick(tickSize, cellProperties, cellModifiers, updatedState, data.modifiers).foreach { msg =>
          context.parent ! msg
        }

        internalAfterTick(tickSize, cellProperties, cellModifiers, updatedState, data.modifiers).foreach { msg =>
          context.parent ! msg
        }

        goto(State.Idle).using(Data(updatedState, initialModifiers))

      case Event(GetActiveEffects(), data) =>
        val activeEffects = effects.collect {
          case (p, effect) if p(properties, data.state) => effect
        }

        stay().replying(activeEffects)
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
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers
  ) extends Message

  case class ApplyEffects(
    tickSize: Int,
    externalEffects: Seq[owe.effects.Effect]
  ) extends Message

  case class GetActiveEffects() extends Message
}
