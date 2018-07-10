package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure.CommoditiesState
import owe.entities.active.behaviour.structure.transformations.ProducedResources
import owe.production.Commodity
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ProducedResourcesSpec extends UnitSpec {
  case class FixtureParam(
    transformer: ProducedResources,
    structure: StructureData
  )

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new ProducedResources {}

    val structure = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state,
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    withFixture(test.toNoArgTest(FixtureParam(transformer, structure)))
  }

  "A ProducedResources transformation" should "update a structure's commodities based on production" in { fixture =>
    val commoditiesState = Fixtures.Structure.Producing.state.commodities.asInstanceOf[CommoditiesState]

    fixture.transformer.withProducedResources(
      structure = fixture.structure
    ) should be(
      fixture.structure.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(25))
        )
      )
    )

    {
      val structureWithCommoditiesAtLimit = fixture.structure.copy(
        state = fixture.structure.state.copy(
          commodities = commoditiesState.copy(
            available = commoditiesState.limits.filter(_._1 == Commodity("TestCommodity"))
          )
        )
      )

      fixture.transformer.withProducedResources(
        structureWithCommoditiesAtLimit
      ) should be(
        structureWithCommoditiesAtLimit.state
      )
    }
  }
}
