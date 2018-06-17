package owe.test.specs.unit.entities.active.behaviour.structure

import org.scalatest.Outcome
import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure.{CommoditiesModifier, CommoditiesState, ProductionState}
import owe.entities.active.behaviour.structure.CommodityCalculations
import owe.production.{Commodity, CommodityAmount}
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class CommodityCalculationsSpec extends UnitSpec {
  case class FixtureParam(structure: StructureData)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(
      test.toNoArgTest(
        FixtureParam(
          StructureData(
            Fixtures.Structure.Producing.properties,
            Fixtures.Structure.Producing.state,
            Fixtures.Structure.Producing.modifiers,
            Fixtures.MockRefs.structure
          )
        )
      )
    )

  "CommodityCalculations" should "calculate production" in { fixture =>
    val productionState = Fixtures.Structure.Producing.state.production.asInstanceOf[ProductionState]
    val commoditiesState = Fixtures.Structure.Producing.state.commodities.asInstanceOf[CommoditiesState]
    val commoditiesModifier = Fixtures.Structure.Producing.modifiers.commodities.asInstanceOf[CommoditiesModifier]

    CommodityCalculations.production(
      fixture.structure
    ) should be(
      Some(
        productionState.rates.filter(_._1 == Commodity("TestCommodity"))
      )
    )

    CommodityCalculations.production(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          production = productionState.copy(employees = 0)
        )
      )
    ) should be(None)

    CommodityCalculations.production(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = commoditiesState.copy(
            available = commoditiesState.limits.filter(_._1 == Commodity("TestCommodity"))
          )
        )
      )
    ) should be(None)

    CommodityCalculations.production(
      fixture.structure.copy(
        modifiers = fixture.structure.modifiers.copy(
          commodities = commoditiesModifier.copy(
            usageRates = commoditiesState.limits.filter(_._1 == Commodity("TestCommodity"))
          )
        )
      )
    ) should be(None)
  }

  it should "calculate consumption" in { fixture =>
    val productionState = Fixtures.Structure.Producing.state.production.asInstanceOf[ProductionState]
    val commoditiesState = Fixtures.Structure.Producing.state.commodities.asInstanceOf[CommoditiesState]
    val commoditiesModifier = Fixtures.Structure.Producing.modifiers.commodities.asInstanceOf[CommoditiesModifier]

    // should succeed
    CommodityCalculations.consumption(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = commoditiesState.copy(
            available = commoditiesState.limits.filter(_._1 == Commodity("TestCommodity"))
          )
        ),
        modifiers = fixture.structure.modifiers.copy(
          commodities = commoditiesModifier.copy(
            usageRates = Map(Commodity("TestCommodity") -> CommodityAmount(10))
          )
        )
      )
    ) should be(Some(Map(Commodity("TestCommodity") -> CommodityAmount(10))))

    // should fail; no usage rates
    CommodityCalculations.consumption(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = commoditiesState.copy(
            available = commoditiesState.limits.filter(_._1 == Commodity("TestCommodity"))
          )
        )
      )
    ) should be(None)

    // should fail; no available commodities
    CommodityCalculations.consumption(
      fixture.structure.copy(
        modifiers = fixture.structure.modifiers.copy(
          commodities = commoditiesModifier.copy(
            usageRates = Map(Commodity("TestCommodity") -> CommodityAmount(10))
          )
        )
      )
    ) should be(None)

    // should fail; no employees
    CommodityCalculations.consumption(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = commoditiesState.copy(
            available = commoditiesState.limits.filter(_._1 == Commodity("TestCommodity"))
          ),
          production = productionState.copy(employees = 0)
        ),
        modifiers = fixture.structure.modifiers.copy(
          commodities = commoditiesModifier.copy(
            usageRates = Map(Commodity("TestCommodity") -> CommodityAmount(10))
          )
        )
      )
    ) should be(None)
  }

  it should "calculate required commodities" in { fixture =>
    val commoditiesState = Fixtures.Structure.Producing.state.commodities.asInstanceOf[CommoditiesState]
    val commoditiesModifier = Fixtures.Structure.Producing.modifiers.commodities.asInstanceOf[CommoditiesModifier]

    // should succeed
    CommodityCalculations.requiredCommodities(
      fixture.structure.copy(
        modifiers = fixture.structure.modifiers.copy(
          commodities = commoditiesModifier.copy(
            usageRates = Map(Commodity("TestCommodity") -> CommodityAmount(10))
          )
        )
      )
    ) should be(
      Some(
        Map(Commodity("TestCommodity") -> CommodityAmount(100))
      )
    )

    // should fail; no usage rates
    CommodityCalculations.requiredCommodities(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = commoditiesState.copy(
            available = commoditiesState.limits.filter(_._1 == Commodity("TestCommodity"))
          )
        )
      )
    ) should be(None)

    // should fail; available commodities are already at their limit
    CommodityCalculations.requiredCommodities(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = commoditiesState.copy(
            available = commoditiesState.limits.filter(_._1 == Commodity("TestCommodity"))
          )
        ),
        modifiers = fixture.structure.modifiers.copy(
          commodities = commoditiesModifier.copy(
            usageRates = Map(Commodity("TestCommodity") -> CommodityAmount(10))
          )
        )
      )
    ) should be(None)
  }

  it should "calculate missing commodities" in { fixture =>
    val commoditiesState = Fixtures.Structure.Producing.state.commodities.asInstanceOf[CommoditiesState]
    val commoditiesModifier = Fixtures.Structure.Producing.modifiers.commodities.asInstanceOf[CommoditiesModifier]

    // should report commodities as not missing
    CommodityCalculations.areHousingCommoditiesMissing(
      fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = commoditiesState.copy(
            available = commoditiesState.limits.filter(_._1 == Commodity("TestCommodity"))
          )
        ),
        modifiers = fixture.structure.modifiers.copy(
          commodities = commoditiesModifier.copy(
            usageRates = Map(Commodity("TestCommodity") -> CommodityAmount(10))
          )
        )
      )
    ) should be(false)

    // should report commodities as missing
    CommodityCalculations.areHousingCommoditiesMissing(
      fixture.structure.copy(
        modifiers = fixture.structure.modifiers.copy(
          commodities = commoditiesModifier.copy(
            usageRates = Map(Commodity("TestCommodity") -> CommodityAmount(10))
          )
        )
      )
    ) should be(true)
  }
}
