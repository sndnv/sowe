package owe.test.specs.unit.entities.definitions.active.walkers

import owe.EntityID
import owe.effects.Effect
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.{ActiveEntityData, WalkerData}
import owe.entities.active.Walker._
import owe.entities.active._
import owe.entities.active.behaviour.walker.{BaseWalker, Carrier}
import owe.map.grid.Point
import owe.test.specs.unit.entities.definitions.active.walkers.Courier.Parameters

import scala.collection.immutable.Queue

class Courier(parameters: Parameters) extends Walker {
  override protected def createActiveEntityData(): ActiveEntity.ActiveEntityData = WalkerData(
    properties = Properties(
      id = java.util.UUID.randomUUID(),
      parent = None,
      homePosition = Point(0, 0),
      name = "Courier",
      maxLife = Life(100),
      movementSpeed = Speed(5),
      maxRoamingDistance = Distance(50),
      attack = NoAttack
    ),
    state = State(
      currentLife = Life(100),
      distanceCovered = Distance(0),
      destinationPath = Queue.empty,
      commodities = CommoditiesState(
        available = Map.empty,
        limits = Map.empty
      ),
      path = Queue.empty,
      mode = MovementMode.Advancing
    ),
    modifiers = StateModifiers(
      movementSpeed = SpeedModifier(75),
      maxRoamingDistance = DistanceModifier(100),
      attack = NoAttack
    )
  )

  override protected def createBehaviour(): BaseWalker = new Carrier {
    override protected def source: EntityID = parameters.source

    override protected def canReturnCommodities: Boolean = true

    override protected def actions: Seq[BaseWalker.Action] = deliver

    override protected def target: EntityID = parameters.target
  }

  override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = Seq.empty
}

object Courier {
  case class Parameters(source: EntityID, target: EntityID)
}
