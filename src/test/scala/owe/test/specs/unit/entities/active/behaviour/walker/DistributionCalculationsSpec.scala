package owe.test.specs.unit.entities.active.behaviour.walker

import org.scalatest.Outcome
import owe.entities.ActiveEntity.{ResourceData, StructureData, WalkerData}
import owe.entities.active.behaviour.walker.DistributionCalculations
import owe.entities.active.behaviour.walker.DistributionCalculations.DistributionResult
import owe.entities.active.{Structure, Walker}
import owe.production.Commodity
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class DistributionCalculationsSpec extends UnitSpec {
  case class FixtureParam(walker: WalkerData, structure: StructureData, resource: ResourceData)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(
      test.toNoArgTest(
        FixtureParam(
          walker = WalkerData(
            Fixtures.Walker.properties,
            Fixtures.Walker.state,
            Fixtures.Walker.modifiers,
            Fixtures.MockRefs.walker
          ),
          structure = StructureData(
            Fixtures.Structure.Producing.properties,
            Fixtures.Structure.Producing.state,
            Fixtures.Structure.Producing.modifiers,
            Fixtures.MockRefs.structure
          ),
          resource = ResourceData(
            Fixtures.Resource.properties,
            Fixtures.Resource.state,
            Fixtures.Resource.modifiers,
            Fixtures.MockRefs.resource
          )
        )
      )
    )

  "A DistributionCalculations" should "calculate transfer from resources to walkers" in { fixture =>
    DistributionCalculations.resourceToWalkerTransfer(
      fixture.resource,
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map.empty,
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(150))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-100)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
        )
      )
    )

    DistributionCalculations.resourceToWalkerTransfer(
      fixture.resource,
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-50)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(50))
        )
      )
    )

    DistributionCalculations.resourceToWalkerTransfer(
      fixture.resource,
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(80)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-20)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(20))
        )
      )
    )

    DistributionCalculations.resourceToWalkerTransfer(
      fixture.resource.copy(
        state = fixture.resource.state.copy(
          currentAmount = Commodity.Amount(10)
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(80)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-10)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(10))
        )
      )
    )

    DistributionCalculations.resourceToWalkerTransfer(
      fixture.resource.copy(
        state = fixture.resource.state.copy(
          currentAmount = Commodity.Amount(0)
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map.empty,
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      None
    )

    DistributionCalculations.resourceToWalkerTransfer(
      fixture.resource.copy(
        state = fixture.resource.state.copy(
          currentAmount = Commodity.Amount(0)
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(80)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      None
    )

    DistributionCalculations.resourceToWalkerTransfer(
      fixture.resource,
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(100)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      None
    )

    DistributionCalculations.resourceToWalkerTransfer(
      fixture.resource.copy(
        state = fixture.resource.state.copy(
          currentAmount = Commodity.Amount(10)
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.NoCommodities
        )
      )
    ) should be(
      None
    )
  }

  it should "calculate transfers from walkers to structures" in { fixture =>
    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map.empty,
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-50)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(50))
        )
      )
    )

    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure,
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-50)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(50))
        )
      )
    )

    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(80)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-20)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(20))
        )
      )
    )

    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure,
      fixture.walker
    ) should be(None)

    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(80)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.NoCommodities
        )
      )
    ) should be(
      None
    )

    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.NoCommodities
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      None
    )

    DistributionCalculations.walkerToStructureTransfer(
      fixture.structure,
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map.empty,
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      None
    )
  }

  it should "calculate transfers from structures to walkers" in { fixture =>
    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map.empty,
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-10)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(10))
        )
      )
    )

    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-10)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(10))
        )
      )
    )

    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(75)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      Some(
        DistributionResult(
          sourceCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(-50)),
          targetCommodities = Map(Commodity("TestCommodity") -> Commodity.Amount(50))
        )
      )
    )

    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(0)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      None
    )

    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(75)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(100)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      None
    )

    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map.empty,
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map.empty,
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      None
    )

    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(75)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.NoCommodities
        )
      )
    ) should be(
      None
    )

    DistributionCalculations.structureToWalkerTransfer(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = Structure.NoCommodities
        )
      ),
      fixture.walker.copy(
        state = fixture.walker.state.copy(
          commodities = Walker.CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    ) should be(
      None
    )
  }
}
