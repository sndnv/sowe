package owe.test.specs.unit.entities.definitions.active.walkers

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, WalkerData}
import owe.entities.active.Walker._
import owe.entities.active._
import owe.entities.active.behaviour.walker
import owe.entities.active.behaviour.walker.BaseWalker
import owe.map.grid.Point

import scala.collection.immutable.Queue

class Recruiter extends Walker {
  override protected def createBehaviour(): BaseWalker = new walker.roaming.Recruiter {
    override protected def recruitmentRadius: Distance = Distance(3)
  }

  override protected def createActiveEntityData(): ActiveEntityData = WalkerData(
    properties = Properties(
      id = java.util.UUID.randomUUID(),
      parent = None,
      homePosition = Point(0, 0),
      name = "Recruiter",
      maxLife = Life(100),
      movementSpeed = Speed(100),
      maxRoamingDistance = Distance(50),
      attack = NoAttack
    ),
    state = State(
      currentLife = Life(100),
      distanceCovered = Distance(0),
      destinationPath = Queue.empty,
      commodities = NoCommodities,
      path = Queue.empty,
      mode = MovementMode.Roaming
    ),
    modifiers = StateModifiers(
      movementSpeed = SpeedModifier(100),
      maxRoamingDistance = DistanceModifier(100),
      attack = NoAttack
    )
  )

  override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = Seq.empty
}
