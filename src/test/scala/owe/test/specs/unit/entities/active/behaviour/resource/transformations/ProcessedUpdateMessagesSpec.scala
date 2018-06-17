package owe.test.specs.unit.entities.active.behaviour.resource.transformations

import org.scalatest.Outcome
import owe.entities.ActiveEntity.ResourceData
import owe.entities.Entity.{ProcessAttack, ProcessCommodities, ProcessLabourFound, ProcessLabourUpdate}
import owe.entities.active.AttackDamage
import owe.entities.active.behaviour.resource.transformations.ProcessedUpdateMessages
import owe.production.CommodityAmount
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ProcessedUpdateMessagesSpec extends UnitSpec {
  case class FixtureParam(
    transformer: ProcessedUpdateMessages,
    resource: ResourceData
  )

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new ProcessedUpdateMessages {}
    val resource = ResourceData(
      Fixtures.Resource.properties,
      Fixtures.Resource.state,
      Fixtures.Resource.modifiers,
      Fixtures.MockRefs.resource
    )

    withFixture(test.toNoArgTest(FixtureParam(transformer, resource)))
  }

  "A ProcessedUpdateMessages transformation" should "handle outgoing commodities" in { fixture =>
    // no messages
    fixture.transformer.withProcessedUpdateMessages(
      resource = fixture.resource,
      pendingMessages = Seq.empty
    ) should be(fixture.resource.state)

    // single, expected message
    fixture.transformer.withProcessedUpdateMessages(
      resource = fixture.resource,
      pendingMessages = Seq(
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(-42))))
      )
    ) should be(fixture.resource.state.copy(currentAmount = CommodityAmount(58)))

    // single, expected but invalid message
    fixture.transformer.withProcessedUpdateMessages(
      resource = fixture.resource,
      pendingMessages = Seq(
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(42))))
      )
    ) should be(fixture.resource.state)

    // multiple, expected messages
    fixture.transformer.withProcessedUpdateMessages(
      resource = fixture.resource,
      pendingMessages = Seq(
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(-1)))),
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(-11)))),
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(-17))))
      )
    ) should be(fixture.resource.state.copy(currentAmount = CommodityAmount(71)))

    // multiple, expected but invalid messages
    fixture.transformer.withProcessedUpdateMessages(
      resource = fixture.resource,
      pendingMessages = Seq(
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(1)))),
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(11)))),
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(17))))
      )
    ) should be(fixture.resource.state)

    // single, unexpected, message
    fixture.transformer.withProcessedUpdateMessages(
      resource = fixture.resource,
      pendingMessages = Seq(
        ProcessAttack(AttackDamage(87))
      )
    ) should be(fixture.resource.state)

    // multiple, unexpected, messages
    fixture.transformer.withProcessedUpdateMessages(
      resource = fixture.resource,
      pendingMessages = Seq(
        ProcessAttack(AttackDamage(87)),
        ProcessLabourFound(),
        ProcessLabourUpdate(1)
      )
    ) should be(fixture.resource.state)

    // multiple, mixed, messages
    fixture.transformer.withProcessedUpdateMessages(
      resource = fixture.resource,
      pendingMessages = Seq(
        ProcessAttack(AttackDamage(87)),
        ProcessLabourFound(),
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(-42)))),
        ProcessLabourUpdate(1)
      )
    ) should be(fixture.resource.state.copy(currentAmount = CommodityAmount(58)))

    // multiple, mixed (valid and invalid) messages
    fixture.transformer.withProcessedUpdateMessages(
      resource = fixture.resource,
      Seq(
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(1)))),
        ProcessAttack(AttackDamage(-87)),
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(-42)))),
        ProcessLabourFound(),
        ProcessCommodities(Seq((fixture.resource.properties.commodity, CommodityAmount(-3)))),
        ProcessLabourUpdate(1)
      )
    ) should be(fixture.resource.state.copy(currentAmount = CommodityAmount(55)))
  }
}
