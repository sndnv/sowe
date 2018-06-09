package owe.test.specs.unit.entities.definitions.active.structures

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, StructureData}
import owe.entities.active.Structure._
import owe.entities.active.behaviour.structure.{housing, BaseStructure}
import owe.entities.active.{Life, RiskAmount, Structure}
import owe.map.grid.Point
import owe.production.{Commodity, CommodityAmount}
import owe.{entities, CellDesirability, EntityDesirability}

class House extends Structure {

  override def `desirability`: EntityDesirability = EntityDesirability.fromInt(-1, 0, 0, 0, 0, 0)

  override protected def createActiveEntityData(): ActiveEntityData = StructureData(
    properties = Properties(
      id = java.util.UUID.randomUUID(),
      homePosition = Point(0, 0),
      name = "House",
      walkers = WalkersProperties(
        generators = Map.empty
      ),
      stages = MultiStage(
        stages = Seq(
          StageProperties(
            maxLife = Life(100),
            maxPeople = 5,
            minDesirability = CellDesirability.Neutral,
            commodityShortageLimit = 10
          ),
          StageProperties(
            maxLife = Life(150),
            maxPeople = 15,
            minDesirability = CellDesirability(4),
            commodityShortageLimit = 5
          ),
          StageProperties(
            maxLife = Life(200),
            maxPeople = 50,
            minDesirability = CellDesirability.Max,
            commodityShortageLimit = 3
          )
        )
      )
    ),
    state = State(
      risk = RiskState(fire = RiskAmount(0), damage = RiskAmount(0)),
      commodities = CommoditiesState(
        available = Map.empty,
        limits = Map(
          Commodity("Food") -> CommodityAmount(100),
          Commodity("Ceramics") -> CommodityAmount(10),
          Commodity("Tea") -> CommodityAmount(200)
        )
      ),
      housing = HousingState(
        occupants = 0,
        education = Map.empty,
        entertainment = Map.empty,
        religion = Map.empty,
        healthcare = Map.empty,
        civilService = Map.empty,
        commodityShortage = 0
      ),
      production = NoProduction,
      currentStage = CurrentStage(0),
      currentLife = Life(100),
      walkers = WalkersState(
        state = Map.empty
      )
    ),
    modifiers = StateModifiers(
      risk = RiskModifier(fire = RiskAmount(3), damage = RiskAmount(5)),
      commodities = CommoditiesModifier(
        usageRates = Map(
          Commodity("Food") -> CommodityAmount(5),
          Commodity("Ceramics") -> CommodityAmount(1),
          Commodity("Tea") -> CommodityAmount(5)
        )
      ),
      production = NoProduction,
      housing = HousingModifier(
        education = Map.empty,
        entertainment = Map.empty,
        religion = Map.empty,
        healthcare = Map.empty,
        civilService = Map.empty
      )
    )
  )

  override protected def createBehaviour(): BaseStructure = new housing.HousingStructure {}

  override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = Seq.empty

  override def `size`: entities.Entity.Size = entities.Entity.Size(height = 1, width = 1)
}
