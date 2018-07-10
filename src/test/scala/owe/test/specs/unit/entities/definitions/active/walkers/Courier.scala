package owe.test.specs.unit.entities.definitions.active.walkers

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, ActiveEntityRef, WalkerData}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker._
import owe.entities.active._
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.acting.Carrier
import owe.map.grid.Point
import owe.test.specs.unit.entities.definitions.active.walkers.Courier.Parameters

import scala.collection.immutable.Queue

class Courier(parameters: Parameters) extends Walker {
  override protected def createActiveEntityData(): ActiveEntityRef => ActiveEntityData = {
    case id: WalkerRef =>
      WalkerData(
        properties = Properties(
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
        ),
        id
      )
  }

  override protected def createBehaviour(): BaseWalker = new Carrier {
    override protected def source: StructureRef = parameters.source

    override protected def canReturnCommodities: Boolean = true

    override protected def actions: Seq[BaseWalker.Action] = deliver

    override protected def target: StructureRef = parameters.target
  }

  override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = Seq.empty
}

object Courier {
  case class Parameters(source: StructureRef, target: StructureRef)
}
