package game.entities.walkers

import game.entities.walkers.Courier.Parameters
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, WalkerData}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker
import owe.entities.active.Walker._
import owe.entities.active.attributes._
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.acting.Carrier
import owe.map.grid.Point
import owe.production.Commodity

import scala.collection.immutable.Queue

class Courier(parent: StructureRef, parentHome: Point, parameters: Parameters) extends Walker {
  override def spawnLocation: SpawnLocation = SpawnLocation.AdjacentPoint(parent)

  override protected def createActiveEntityData(): ActiveEntityRef => Data = {
    case id: WalkerRef =>
      WalkerData(
        properties = Properties(
          parent = Some(parent),
          homePosition = parentHome,
          name = "Courier",
          maxLife = Life(100),
          movementSpeed = Speed(1),
          maxRoamingDistance = Distance(100),
          attack = NoAttack,
          traversalMode = TraversalMode.OnLand
        ),
        state = State(
          currentPosition = Point(0, 0),
          currentLife = Life(100),
          distanceCovered = Distance(0),
          commodities = CommoditiesState(
            available = Map.empty,
            limits = Map(
              Commodity("Tea") -> Commodity.Amount(100),
              Commodity("Copper") -> Commodity.Amount(2)
            )
          ),
          path = Queue.empty,
          mode = MovementMode.Roaming
        ),
        modifiers = StateModifiers(
          movementSpeed = Speed.Modifier(100),
          maxRoamingDistance = Distance.Modifier(100),
          attack = NoAttack
        ),
        id
      )
  }

  override protected def createBehaviour(): BaseWalker = new Carrier {
    override protected def canReturnCommodities: Boolean = true
    override protected def actions: Seq[BaseWalker.Action] = deliver(parameters.source, parameters.target)
  }

  override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
}

object Courier {
  case class Parameters(source: StructureRef, target: StructureRef)
}
