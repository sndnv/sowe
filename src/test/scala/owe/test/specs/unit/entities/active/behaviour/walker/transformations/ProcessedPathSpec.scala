package owe.test.specs.unit.entities.active.behaviour.walker.transformations

import org.scalatest.FutureOutcome
import owe.entities.ActiveEntity.WalkerData
import owe.entities.active.behaviour.walker.transformations.ProcessedPath
import owe.map.grid.Point
import owe.test.specs.unit.AsyncUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

import scala.collection.immutable.Queue

class ProcessedPathSpec extends AsyncUnitSpec {
  case class FixtureParam(transformer: ProcessedPath, walker: WalkerData)

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val transformer = new ProcessedPath {}

    val walker = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    withFixture(test.toNoArgAsyncTest(FixtureParam(transformer, walker)))
  }

  "A ProcessedPath transformation" should "update a walker's path" in { fixture =>
    for {
      updatedStateWithEmptyPath <- fixture.transformer.withProcessedPath(
        fixture.walker,
        updatedPath = Queue.empty
      )
      updatedStateWithPath <- fixture.transformer.withProcessedPath(
        fixture.walker,
        updatedPath = Queue(Point(0, 1), Point(1, 1), Point(1, 2))
      )
    } yield {
      updatedStateWithEmptyPath should be(
        fixture.walker.state.copy(path = Queue.empty)
      )
      updatedStateWithPath should be(
        fixture.walker.state.copy(path = Queue(Point(0, 1), Point(1, 1), Point(1, 2)))
      )
    }
  }
}
