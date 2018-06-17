package owe.test.specs.unit.entities.active.behaviour.walker.transformations

import org.scalatest.FutureOutcome
import owe.entities.ActiveEntity.WalkerData
import owe.entities.active.Life
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.entities.active.behaviour.walker.transformations.RoamAction
import owe.map.grid.Point
import owe.test.specs.unit.AsyncUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

import scala.concurrent.Future

class RoamActionSpec extends AsyncUnitSpec {
  case class FixtureParam(transformer: RoamAction, walker: WalkerData)

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val transformer = new RoamAction {}

    val walker = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    withFixture(test.toNoArgAsyncTest(FixtureParam(transformer, walker)))
  }

  "A RoamAction transformation" should "process one-time operations" in { fixture =>
    for {
      updatedState <- fixture.transformer.withRoamAction(
        fixture.walker,
        DoOperation { data =>
          Future.successful(data.state.copy(currentLife = Life(42)))
        }
      )
    } yield {
      updatedState should be(fixture.walker.state.copy(currentLife = Life(42)))
    }
  }

  it should "process repeatable operations" in { fixture =>
    for {
      updatedState <- fixture.transformer.withRoamAction(
        fixture.walker,
        DoRepeatableOperation(
          data => Future.successful(data.state.copy(currentLife = Life(42))),
          _ => true
        )
      )
    } yield {
      updatedState should be(fixture.walker.state.copy(currentLife = Life(42)))
    }
  }

  it should "not process other operations" in { fixture =>
    for {
      updatedStateNoAction <- fixture.transformer.withRoamAction(
        fixture.walker,
        NoAction
      )
      updatedStateGoToPoint <- fixture.transformer.withRoamAction(
        fixture.walker,
        GoToPoint(Point(0, 0))
      )
      updatedStateGoToEntity <- fixture.transformer.withRoamAction(
        fixture.walker,
        GoToEntity(fixture.walker.id)
      )
      updatedStateGoHome <- fixture.transformer.withRoamAction(
        fixture.walker,
        GoHome()
      )
    } yield {
      updatedStateNoAction should be(fixture.walker.state)
      updatedStateGoToPoint should be(fixture.walker.state)
      updatedStateGoToEntity should be(fixture.walker.state)
      updatedStateGoHome should be(fixture.walker.state)
    }
  }
}
