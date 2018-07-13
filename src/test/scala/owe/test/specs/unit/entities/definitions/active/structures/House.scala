package owe.test.specs.unit.entities.definitions.active.structures

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, StructureData}
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.active.Structure
import owe.entities.active.Structure._
import owe.entities.active.behaviour.structure.{housing, BaseStructure}
import owe.entities.active.attributes.{Life, RiskAmount}
import owe.map.Cell
import owe.map.grid.Point
import owe.production.Commodity

class House extends Structure {

  override def `desirability`: Desirability = Desirability.fromInt(-1, 0, 0, 0, 0, 0)

  override protected def createActiveEntityData(): ActiveEntityRef => Data = {
    case id: StructureRef =>
      StructureData(
        properties = Properties(
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
                minDesirability = Cell.Desirability.Neutral,
                commodityShortageLimit = 10
              ),
              StageProperties(
                maxLife = Life(150),
                maxPeople = 15,
                minDesirability = Cell.Desirability(4),
                commodityShortageLimit = 5
              ),
              StageProperties(
                maxLife = Life(200),
                maxPeople = 50,
                minDesirability = Cell.Desirability.Max,
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
              Commodity("Food") -> Commodity.Amount(100),
              Commodity("Ceramics") -> Commodity.Amount(10),
              Commodity("Tea") -> Commodity.Amount(200)
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
              Commodity("Food") -> Commodity.Amount(5),
              Commodity("Ceramics") -> Commodity.Amount(1),
              Commodity("Tea") -> Commodity.Amount(5)
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
        ),
        id
      )
  }

  override protected def createBehaviour(): BaseStructure = new housing.HousingStructure {}

  override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty

  override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
}
