package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.entities.ActiveEntity.{MapData, StructureData}
import owe.entities.active.Structure.CommoditiesState
import owe.entities.active.behaviour.structure.transformations.ProcessedFarming
import owe.map.Cell.{Fertility, Water}
import owe.production.Commodity
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ProcessedFarmingSpec extends UnitSpec {
  case class FixtureParam(
    transformer: ProcessedFarming,
    structure: StructureData,
    map: MapData
  )

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new ProcessedFarming {}

    val structure = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state,
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    val map = Fixtures.defaultMapData

    withFixture(test.toNoArgTest(FixtureParam(transformer, structure, map)))
  }

  "A ProcessedFarming transformation" should "update a structure's commodities based on production" in { fixture =>
    val commoditiesState = fixture.structure.state.commodities.asInstanceOf[CommoditiesState]

    fixture.transformer.withProcessedFarming(
      structure = fixture.structure,
      map = fixture.map
    ) should be(
      fixture.structure.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(25))
        )
      )
    )

    fixture.transformer.withProcessedFarming(
      structure = fixture.structure,
      map = fixture.map.copy(
        cellState = fixture.map.cellState.copy(fertility = Fertility(50))
      )
    ) should be(
      fixture.structure.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(12))
        )
      )
    )

    fixture.transformer.withProcessedFarming(
      structure = fixture.structure,
      map = fixture.map.copy(
        cellState = fixture.map.cellState.copy(water = Water(50))
      )
    ) should be(
      fixture.structure.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(12))
        )
      )
    )

    fixture.transformer.withProcessedFarming(
      structure = fixture.structure,
      map = fixture.map.copy(
        cellState = fixture.map.cellState.copy(water = Water(50), fertility = Fertility(50))
      )
    ) should be(
      fixture.structure.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(6))
        )
      )
    )
  }
}
