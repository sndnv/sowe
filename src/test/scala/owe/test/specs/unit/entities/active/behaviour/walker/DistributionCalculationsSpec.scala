package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.entities.ActiveEntity.{StructureData, WalkerData}
import owe.entities.active.behaviour.walker.DistributionCalculations
import owe.entities.active.behaviour.walker.DistributionCalculations.DistributionResult
import owe.entities.active.{Structure, Walker}
import owe.production.{Commodity, CommodityAmount}
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class DistributionCalculationsSpec extends UnitSpec {
  case class FixtureParam(walker: WalkerData, structure: StructureData)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(
      test.toNoArgTest(
        FixtureParam(
          WalkerData(
            Fixtures.Walker.properties,
            Fixtures.Walker.state,
            Fixtures.Walker.modifiers,
            Fixtures.MockRefs.walker
          ),
          StructureData(
            Fixtures.Structure.Producing.properties,
            Fixtures.Structure.Producing.state,
            Fixtures.Structure.Producing.modifiers,
            Fixtures.MockRefs.structure
          )
        )
      )
    )

  "A DistributionCalculations" should "calculate transfers from walkers to structures" in { fixture =>
    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure,
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(50)),
            limits = Map(Commodity("TestCommodity") -> CommodityAmount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          structureCommodities = Map(Commodity("TestCommodity") -> CommodityAmount(50)),
          walkerCommodities = Map(Commodity("TestCommodity") -> CommodityAmount(-50))
        )
      )
    )

    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(80)),
            limits = Map(Commodity("TestCommodity") -> CommodityAmount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(50)),
            limits = Map(Commodity("TestCommodity") -> CommodityAmount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          structureCommodities = Map(Commodity("TestCommodity") -> CommodityAmount(20)),
          walkerCommodities = Map(Commodity("TestCommodity") -> CommodityAmount(-20))
        )
      )
    )

    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure,
      fixture.walker
    ) should be(None)
  }

  it should "calculate transfers from structures to walkers" in { fixture =>
    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(10)),
            limits = Map(Commodity("TestCommodity") -> CommodityAmount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(50)),
            limits = Map(Commodity("TestCommodity") -> CommodityAmount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          structureCommodities = Map(Commodity("TestCommodity") -> CommodityAmount(-10)),
          walkerCommodities = Map(Commodity("TestCommodity") -> CommodityAmount(10))
        )
      )
    )

    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(75)),
            limits = Map(Commodity("TestCommodity") -> CommodityAmount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(50)),
            limits = Map(Commodity("TestCommodity") -> CommodityAmount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          structureCommodities = Map(Commodity("TestCommodity") -> CommodityAmount(-50)),
          walkerCommodities = Map(Commodity("TestCommodity") -> CommodityAmount(50))
        )
      )
    )
  }
}
