package owe.entities

import scala.reflect.ClassTag

import akka.actor.{Actor, ActorLogging, Props}
import owe.EffectID

abstract class ActiveEntity[
  P <: Entity.Properties,
  S <: Entity.State,
  M <: Entity.StateModifiers: ClassTag
]() extends Entity {
  type Effect = Entity.Effect[P, S, M]

  protected def beforeTick(tickSize: Int, state: S, modifiers: M): Seq[owe.Message] = Seq.empty
  protected def afterTick(tickSize: Int, state: S, modifiers: M): Seq[owe.Message] = Seq.empty
  protected def beforeEffects(state: S, modifiers: M): Seq[owe.Message] = Seq.empty
  protected def afterEffects(state: S, modifiers: M): Seq[owe.Message] = Seq.empty
  protected def createProperties(): P
  protected def createState(): S
  protected def createStateModifiers(): M
  protected def createEffects(): Map[EffectID, Effect]
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
    var modifiers: M
  ) extends Actor
      with ActorLogging {

    override def receive: Receive = {
      case ActiveEntity.ProcessTick(tickSize) =>
        beforeTick(tickSize, state, modifiers).foreach(msg => context.parent ! msg)
        state = tick(tickSize, state, modifiers)
        afterTick(tickSize, state, modifiers).foreach(msg => context.parent ! msg)

      case ActiveEntity.ApplyEffects(effects) =>
        beforeEffects(state, modifiers).foreach(msg => context.parent ! msg)

        effects.foreach { effect =>
          effect(properties, state, modifiers) match {
            case updatedModifiers: M =>
              modifiers = updatedModifiers

            case unexpectedModifiers =>
              log.debug(
                "Unexpected modifiers type [{}] returned by effect [{}]",
                unexpectedModifiers.getClass.getName,
                effect.getClass.getName
              )
          }
        }

        afterEffects(state, modifiers).foreach(msg => context.parent ! msg)
    }
  }
}

object ActiveEntity {

  sealed trait Message extends owe.Message

  case class ProcessTick(tickSize: Int) extends Message

  case class ApplyEffects[
    P <: Entity.Properties,
    S <: Entity.State,
    M <: Entity.StateModifiers: ClassTag
  ](effects: Seq[Entity.Effect[P, S, M]])
      extends Message

}
