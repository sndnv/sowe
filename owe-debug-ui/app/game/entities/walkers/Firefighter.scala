package game.entities.walkers

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, WalkerData}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker
import owe.entities.active.Walker._
import owe.entities.active.attributes.{Distance, Life, Speed}
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.roaming.PublicSafetyOfficial
import owe.map.grid.Point

import scala.collection.immutable.Queue

class Firefighter(parent: StructureRef, home: Point) extends Walker {
  override def spawnLocation: SpawnLocation = SpawnLocation.AdjacentRoad(parent)

  override protected def createBehaviour(): BaseWalker = new PublicSafetyOfficial {}

  override protected def createActiveEntityData(): ActiveEntityRef => Data = {
    case id: WalkerRef =>
      WalkerData(
        properties = Properties(
          parent = Some(parent),
          homePosition = home,
          name = "Firefighter",
          maxLife = Life(100),
          movementSpeed = Speed(1),
          maxRoamingDistance = Distance(50),
          attack = NoAttack,
          traversalMode = TraversalMode.OnLand
        ),
        state = State(
          currentPosition = home,
          currentLife = Life(100),
          distanceCovered = Distance(0),
          commodities = NoCommodities,
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

  override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
}
