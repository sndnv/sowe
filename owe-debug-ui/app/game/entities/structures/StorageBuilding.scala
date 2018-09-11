package game.entities.structures

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, StructureData}
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.active.Structure._
import owe.entities.active.attributes.Life
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.structure.producing.Industry
import owe.entities.active.{Structure, Walker}
import owe.map.Cell
import owe.map.grid.Point
import owe.production.Commodity

class StorageBuilding(home: Point, walkerGenerators: Map[String, StructureData => Option[Walker]]) extends Structure {

  override def `desirability`: Desirability = Desirability.fromInt(-5, -5, -3, -3, -1, -1)

  override def createActiveEntityData(): ActiveEntityRef => Data = {
    case id: StructureRef =>
      StructureData(
        properties = Properties(
          homePosition = home,
          name = "StorageBuilding",
          walkers = WalkersProperties(
            generators = walkerGenerators
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
          commodities = CommoditiesState(
            available = Map(
              Commodity("Tea") -> Commodity.Amount(0),
              Commodity("Copper") -> Commodity.Amount(0)
            ),
            limits = Map(
              Commodity("Tea") -> Commodity.Amount(100),
              Commodity("Copper") -> Commodity.Amount(4)
            )
          ),
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

  override protected def createBehaviour(): BaseStructure = new Industry {}

  override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty

  override def `size`: Entity.Size = StorageBuilding.size
}

object StorageBuilding {
  def size: Entity.Size = Entity.Size(height = 4, width = 4)
}
