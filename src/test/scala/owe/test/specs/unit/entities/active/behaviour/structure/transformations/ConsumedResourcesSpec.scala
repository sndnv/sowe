package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure.{CommoditiesModifier, CommoditiesState}
import owe.entities.active.behaviour.structure.transformations.ConsumedResources
import owe.production.{Commodity, CommodityAmount}
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ConsumedResourcesSpec extends UnitSpec {
  case class FixtureParam(
    transformer: ConsumedResources,
    structure: StructureData
  )

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new ConsumedResources {}

    val structure = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state,
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    withFixture(test.toNoArgTest(FixtureParam(transformer, structure)))
  }

  "A ConsumedResources transformation" should "update a structure's commodities based on consumption" in { fixture =>
    val commoditiesState = Fixtures.Structure.Producing.state.commodities.asInstanceOf[CommoditiesState]
    val commoditiesModifier = Fixtures.Structure.Producing.modifiers.commodities.asInstanceOf[CommoditiesModifier]

    fixture.transformer.withConsumedResources(
      structure = fixture.structure.copy(
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
    ) should be(
      fixture.structure.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> CommodityAmount(90))
        )
      )
    )

    fixture.transformer.withConsumedResources(
      structure = fixture.structure
    ) should be(
      fixture.structure.state
    )
  }
}
