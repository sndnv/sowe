package owe.test.specs.unit.entities.active.behaviour.resource

import org.scalatest.Outcome
import owe.entities.ActiveEntity.ResourceData
import owe.entities.active.behaviour.resource.CommodityCalculations
import owe.production.CommodityAmount
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class CommodityCalculationsSpec extends UnitSpec {
  case class FixtureParam(resource: ResourceData)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(
      test.toNoArgTest(
        FixtureParam(ResourceData(Fixtures.Resource.properties, Fixtures.Resource.state, Fixtures.Resource.modifiers))
      )
    )

  "CommodityCalculations" should "calculate amount produced" in { fixture =>
    CommodityCalculations.amountProduced(
      fixture.resource
    ) should be(Some(fixture.resource.state.replenishAmount))

    CommodityCalculations.amountProduced(
      fixture.resource.copy(
        state = fixture.resource.state.copy(
          currentAmount = fixture.resource.properties.maxAmount - CommodityAmount(11)
        )
      )
    ) should be(Some(CommodityAmount(11)))

    CommodityCalculations.amountProduced(
      fixture.resource.copy(
        state = fixture.resource.state.copy(
          currentAmount = fixture.resource.properties.maxAmount
        )
      )
    ) should be(None)
  }
}
