package owe.test.specs.unit.entities.definitions.active.walkers

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, ActiveEntityRef, WalkerData}
import owe.entities.active.Walker._
import owe.entities.active._
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.idling.Military
import owe.map.grid.Point

import scala.collection.immutable.Queue

class Archer extends Walker {
  override protected def createBehaviour(): BaseWalker = new Military {}

  override protected def createActiveEntityData(): ActiveEntityRef => ActiveEntityData = {
    case id: WalkerRef =>
      WalkerData(
        properties = Properties(
          parent = None,
          homePosition = Point(0, 0),
          name = "Archer",
          maxLife = Life(500),
          movementSpeed = Speed(150),
          maxRoamingDistance = Distance(50),
          attack = AttackProperties(
            rate = AttackRate(3),
            damage = AttackDamage(50),
            distance = Distance(25),
            target = _ => true
          )
        ),
        state = State(
          currentLife = Life(100),
          distanceCovered = Distance(0),
          commodities = NoCommodities,
          path = Queue.empty,
          mode = MovementMode.Advancing
        ),
        modifiers = StateModifiers(
          movementSpeed = SpeedModifier(100),
          maxRoamingDistance = DistanceModifier(100),
          attack = AttackModifiers(
            rate = AttackRateModifier(200),
            damage = AttackDamageModifier(50),
            distance = DistanceModifier(100)
          )
        ),
        id
      )
  }

  override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = Seq.empty
}
