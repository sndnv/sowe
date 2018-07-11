package owe.test.specs.unit.entities.active.behaviour.walker.transformations

import org.scalatest.FutureOutcome
import owe.entities.ActiveEntity.WalkerData
import owe.entities.active.attributes.Distance
import owe.entities.active.Walker.MovementMode
import owe.entities.active.behaviour.walker.transformations.ProcessedMovement
import owe.test.specs.unit.AsyncUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ProcessedMovementSpec extends AsyncUnitSpec {
  case class FixtureParam(transformer: ProcessedMovement, walker: WalkerData)

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val transformer = new ProcessedMovement {}

    val walker = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    withFixture(test.toNoArgAsyncTest(FixtureParam(transformer, walker)))
  }

  "A ProcessedMovement transformation" should "increase a walker's covered distance" in { fixture =>
    for {
      updatedState <- fixture.transformer.withProcessedMovement(
        fixture.walker
      )
      updatedState <- fixture.transformer.withProcessedMovement(
        fixture.walker.copy(state = updatedState)
      )
      updatedState <- fixture.transformer.withProcessedMovement(
        fixture.walker.copy(state = updatedState)
      )
    } yield {
      updatedState should be(
        fixture.walker.state.copy(distanceCovered = fixture.walker.state.distanceCovered + Distance(3))
      )
    }
  }

  it should "update a walker's movement mode" in { fixture =>
    for {
      updatedStateWIthAdvancingMode <- fixture.transformer.withMovementMode(
        fixture.walker,
        newMode = MovementMode.Advancing
      )
      updatedStateWithIdlingMode <- fixture.transformer.withMovementMode(
        fixture.walker,
        newMode = MovementMode.Idling
      )
      updatedStateWithReturningMode <- fixture.transformer.withMovementMode(
        fixture.walker,
        newMode = MovementMode.Returning
      )
      updatedStateWithRoamingMode <- fixture.transformer.withMovementMode(
        fixture.walker,
        newMode = MovementMode.Roaming
      )
    } yield {
      updatedStateWIthAdvancingMode should be(
        fixture.walker.state.copy(mode = MovementMode.Advancing)
      )

      updatedStateWithIdlingMode should be(
        fixture.walker.state.copy(mode = MovementMode.Idling)
      )

      updatedStateWithReturningMode should be(
        fixture.walker.state.copy(mode = MovementMode.Returning)
      )

      updatedStateWithRoamingMode should be(
        fixture.walker.state.copy(mode = MovementMode.Roaming)
      )
    }
  }
}
