package owe.test.specs.unit.entities.definitions.active.structures

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, ActiveEntityRef, StructureData}
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.active.Structure._
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.structure.producing.Industry
import owe.entities.active.{Life, RiskAmount, Structure}
import owe.map.Cell
import owe.map.grid.Point
import owe.production.Commodity

class StorageBuilding extends Structure {

  override def `desirability`: Desirability = Desirability.fromInt(-5, -5, -3, -3, -1, -1)

  override def createActiveEntityData(): ActiveEntityRef => ActiveEntityData = {
    case id: StructureRef =>
      StructureData(
        properties = Properties(
          homePosition = Point(0, 0),
          name = "StorageBuilding",
          walkers = WalkersProperties(
            generators = Map.empty
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
          risk = RiskState(fire = RiskAmount(0), damage = RiskAmount(0)),
          commodities = CommoditiesState(
            available = Map(
              Commodity("Wood") -> Commodity.Amount(25)
            ),
            limits = Map(
              Commodity("Wood") -> Commodity.Amount(100),
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
          risk = RiskModifier(fire = RiskAmount(2), damage = RiskAmount(1)),
          commodities = NoCommodities,
          production = NoProduction,
          housing = NoHousing
        ),
        id
      )
  }

  override protected def createBehaviour(): BaseStructure = new Industry {}

  override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = Seq.empty

  override def `size`: Entity.Size = Entity.Size(height = 4, width = 4)
}
