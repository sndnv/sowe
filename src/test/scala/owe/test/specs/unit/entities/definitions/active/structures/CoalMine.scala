package owe.test.specs.unit.entities.definitions.active.structures

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, StructureData}
import owe.entities.active.Structure._
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.structure.producing.Industry
import owe.entities.active.{Life, RiskAmount, Structure, Walker}
import owe.map.grid.Point
import owe.production.{Commodity, CommodityAmount, CommodityAmountModifier}
import owe.test.specs.unit.entities.definitions.active.walkers.Recruiter
import owe.{entities, CellDesirability, EntityDesirability}

class CoalMine extends Structure {

  private def generateRecruiter(structure: StructureData): Option[Walker] =
    structure.state.production match {
      case ProductionState(_, labour, _) =>
        labour match {
          case LabourState.None    => Some(new Recruiter())
          case LabourState.Found   => None //walker not needed
          case LabourState.Looking => None //walker already out
        }

      case _ => None
    }

  private def generateCourier(structure: StructureData): Option[Walker] = ??? //TODO

  override def `size`: entities.Entity.Size = entities.Entity.Size(height = 3, width = 2)

  override def `desirability`: EntityDesirability = EntityDesirability.fromInt(-5, -5, -3, -3, -3, -3)

  override protected def createActiveEntityData(): ActiveEntityActorRef => ActiveEntityData = { id =>
    StructureData(
      properties = Properties(
        homePosition = Point(0, 0),
        name = "CoalMine",
        walkers = WalkersProperties(
          generators = Map(
            "recruiter" -> generateRecruiter,
            "courier" -> generateCourier
          )
        ),
        stages = SingleStage(
          stage = StageProperties(
            maxLife = Life(100),
            maxPeople = 15,
            minDesirability = CellDesirability.Neutral,
            commodityShortageLimit = 0
          )
        )
      ),
      state = State(
        risk = RiskState(fire = RiskAmount(0), damage = RiskAmount(0)),
        commodities = CommoditiesState(
          available = Map.empty,
          limits = Map(Commodity("Coal") -> CommodityAmount(100))
        ),
        housing = NoHousing,
        production = ProductionState(
          employees = 0,
          labour = LabourState.None,
          rates = Map(Commodity("Coal") -> CommodityAmount(25))
        ),
        currentStage = DefaultStage,
        currentLife = Life(100),
        walkers = WalkersState(
          state = Map.empty
        )
      ),
      modifiers = StateModifiers(
        risk = RiskModifier(fire = RiskAmount(3), damage = RiskAmount(5)),
        commodities = NoCommodities,
        production = ProductionModifier(rates = Map(Commodity("Coal") -> CommodityAmountModifier(100))),
        housing = NoHousing
      ),
      id
    )
  }

  override protected def createBehaviour(): BaseStructure = new Industry {}

  override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = Seq.empty
}
