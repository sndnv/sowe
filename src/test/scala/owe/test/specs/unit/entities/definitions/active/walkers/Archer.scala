package owe.test.specs.unit.entities.definitions.active.walkers

import owe.EffectID
import owe.effects.Effect
import owe.entities.active.Walker

object Archer extends Walker {
  override protected def createProperties(): Walker.Properties = Walker.Properties(
    name = "Archer",
    interactionDistance = 10,
    patrolDistance = Some(25),
    movementSpeed = 10,
    maxLife = 500,
    attackRate = Some(3),
    attackDamage = Some(50)
  )

  override protected def createState(): Walker.State = Walker.State(
    currentLife = 100,
    availableCommodities = Map.empty
  )

  override protected def createStateModifiers(): Walker.StateModifiers = Walker.StateModifiers(
    interactionDistance = 50,
    patrolDistance = None,
    movementSpeed = 150,
    attackRate = Some(200),
    attackDamage = Some(50)
  )

  override protected def createEffects(): Seq[((Walker.Properties, Walker.State) => Boolean, Effect)] = Seq.empty

  override protected def tick(
    tickSize: Int,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Walker.State = ??? //TODO

  override protected def processMovement(
    tickSize: Int,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Seq[owe.Message] = ??? //TODO
}
