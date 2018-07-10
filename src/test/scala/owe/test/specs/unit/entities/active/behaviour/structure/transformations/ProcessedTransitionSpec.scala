package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.entities.ActiveEntity.{MapData, StructureData}
import owe.entities.active.Life
import owe.entities.active.Structure.{CurrentStage, MultiStage, StageProperties}
import owe.entities.active.behaviour.structure.transformations.ProcessedTransition
import owe.map.Cell
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ProcessedTransitionSpec extends UnitSpec {
  case class FixtureParam(
    transformer: ProcessedTransition,
    structure: StructureData,
    map: MapData
  )

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new ProcessedTransition {}

    val multipleStage = MultiStage(
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

    val structure = StructureData(
      Fixtures.Structure.Producing.properties.copy(stages = multipleStage),
      Fixtures.Structure.Producing.state.copy(currentStage = CurrentStage(1)),
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    val map = Fixtures.defaultMapData.copy(
      cellState = Fixtures.defaultCellState.copy(desirability = Cell.Desirability(4))
    )

    withFixture(test.toNoArgTest(FixtureParam(transformer, structure, map)))
  }

  "A ProcessedTransition transformation" should "upgrade a structure" in { fixture =>
    fixture.transformer.withProcessedTransition(
      fixture.map.copy(
        cellState = fixture.map.cellState.copy(desirability = Cell.Desirability.Max)
      ),
      fixture.structure
    ) should be(
      fixture.structure.state.copy(currentStage = CurrentStage(2))
    )
  }

  it should "downgrade a structure" in { fixture =>
    fixture.transformer.withProcessedTransition(
      fixture.map.copy(
        cellState = fixture.map.cellState.copy(desirability = Cell.Desirability.Min)
      ),
      fixture.structure
    ) should be(
      fixture.structure.state.copy(currentStage = CurrentStage(0))
    )
  }

  it should "keep a structure's stage" in { fixture =>
    fixture.transformer.withProcessedTransition(
      fixture.map,
      fixture.structure
    ) should be(
      fixture.structure.state
    )
  }
}
