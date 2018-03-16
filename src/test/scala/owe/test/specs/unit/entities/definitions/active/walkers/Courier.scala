package owe.test.specs.unit.entities.definitions.active.walkers

import owe.effects.Effect
import owe.entities.active.Walker
import owe.map.MapCell

object Courier extends Walker {
  override protected def createProperties(): Walker.Properties = Walker.Properties(
    name = "Courier",
    interactionDistance = 1,
    patrolDistance = None,
    movementSpeed = 5,
    maxLife = 100,
    attackRate = None,
    attackDamage = None
  )

  override protected def createState(): Walker.State = Walker.State(
    currentLife = 100,
    availableCommodities = Map.empty
  )

  override protected def createStateModifiers(): Walker.StateModifiers = Walker.StateModifiers(
    interactionDistance = 50,
    patrolDistance = None,
    movementSpeed = 75,
    attackRate = None,
    attackDamage = None
  )

  override protected def createEffects(): Seq[((Walker.Properties, Walker.State) => Boolean, Effect)] = Seq.empty

  override protected def tick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    properties: Walker.Properties,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Walker.State = ??? //TODO

  override protected def processMovement(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Walker.State,
    modifiers: Walker.StateModifiers
  ): Seq[owe.Message] = ??? //TODO

}
