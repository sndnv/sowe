package owe.test.specs.unit.entities.active.behaviour.resource.transformations

import org.scalatest.Outcome
import owe.entities.ActiveEntity.ResourceData
import owe.entities.active.behaviour.resource.transformations.ReplenishedResources
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ReplenishedResourcesSpec extends UnitSpec {
  case class FixtureParam(
    transformer: ReplenishedResources,
    resource: ResourceData
  )

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new ReplenishedResources {}

    val resource = ResourceData(
      Fixtures.Resource.properties,
      Fixtures.Resource.state,
      Fixtures.Resource.modifiers,
      Fixtures.MockRefs.resource
    )

    withFixture(test.toNoArgTest(FixtureParam(transformer, resource)))
  }

  "A ReplenishedResources transformation" should "add amount produced" in { fixture =>
    fixture.transformer.withReplenishedResources(
      resource = fixture.resource,
      amountProduced = None
    ) should be(fixture.resource.state)

    fixture.transformer.withReplenishedResources(
      resource = fixture.resource,
      amountProduced = Some(fixture.resource.state.replenishAmount)
    ) should be(
      fixture.resource.state.copy(
        currentAmount = fixture.resource.state.currentAmount + fixture.resource.state.replenishAmount
      )
    )
  }
}
