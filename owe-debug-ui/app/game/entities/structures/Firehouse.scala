package game.entities.structures

import game.entities.walkers.Firefighter
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, StructureData}
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.active.Structure._
import owe.entities.active.attributes.Life
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.structure.producing.CivilService
import owe.entities.active.{Structure, Walker}
import owe.map.Cell
import owe.map.grid.Point

class Firehouse(home: Point) extends Structure {

  private def walkerGenerators(parent: StructureRef): Map[String, StructureData => Option[Walker]] = Map(
    "firefighter" -> { _ =>
      Some(new Firefighter(parent, home))
    }
  )

  override def `desirability`: Desirability = Desirability.fromInt(1, 1, 0, 0, 0, 0)

  override def createActiveEntityData(): ActiveEntityRef => Data = {
    case id: StructureRef =>
      StructureData(
        properties = Properties(
          homePosition = home,
          name = "Firehouse",
          walkers = WalkersProperties(
            generators = walkerGenerators(id)
          ),
          stages = SingleStage(
            stage = StageProperties(
              maxLife = Life(100),
              maxPeople = 10,
              minDesirability = Cell.Desirability.Min,
              commodityShortageLimit = 0
            )
          )
        ),
        state = State(
          risk = NoRisk,
          commodities = NoCommodities,
          housing = NoHousing,
          production = NoProduction,
          currentStage = DefaultStage,
          currentLife = Life(100),
          walkers = WalkersState(
            state = Map.empty
          )
        ),
        modifiers = StateModifiers(
          risk = NoRisk,
          commodities = NoCommodities,
          production = NoProduction,
          housing = NoHousing
        ),
        id
      )
  }

  override protected def createBehaviour(): BaseStructure = new CivilService {}

  override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty

  override def `size`: Entity.Size = Firehouse.size
}

object Firehouse {
  def size: Entity.Size = Entity.Size(height = 2, width = 2)
}
