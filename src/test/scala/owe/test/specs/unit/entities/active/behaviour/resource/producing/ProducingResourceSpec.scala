package owe.test.specs.unit.entities.active.behaviour.resource.producing

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity.{ForwardMessage, ProcessEntityTick, ResourceData}
import owe.entities.Entity.ProcessCommodities
import owe.entities.active.behaviour.resource.producing.ProducingResource
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Exchange.{CommodityAvailable, UpdateCommodityState}
import owe.production.{CommodityAmount, CommodityState}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, TestParentEntity}

import scala.concurrent.duration._

class ProducingResourceSpec extends AkkaUnitSpec("ProducingResourceSpec") {

  private implicit val timeout: Timeout = 5.seconds

  case class FixtureParam(parentEntity: ActorRef)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(
      test.toNoArgTest(
        FixtureParam(parentEntity = system.actorOf(TestParentEntity.props(testActor, Props(new ProducingResource {}))))
      )
    )

  "A ProducingResource" should "produce commodities" in { fixture =>
    fixture.parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = ResourceData(
        Fixtures.Resource.properties,
        Fixtures.Resource.state,
        Fixtures.Resource.modifiers
      ),
      messages = Seq.empty
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Fixtures.Resource.properties.commodity,
            Fixtures.Resource.state.replenishAmount,
            CommodityState.Produced
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(
            Fixtures.Resource.properties.commodity,
            Fixtures.Resource.state.currentAmount + Fixtures.Resource.state.replenishAmount,
            Fixtures.Resource.properties.id
          )
        )
      )
    )

    expectMsg(
      Fixtures.Resource.state
        .copy(currentAmount = Fixtures.Resource.state.currentAmount + Fixtures.Resource.state.replenishAmount)
    )
  }

  it should "allow commodities to be retrieved by walkers" in { fixture =>
    fixture.parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = ResourceData(
        Fixtures.Resource.properties,
        Fixtures.Resource.state,
        Fixtures.Resource.modifiers
      ),
      messages = Seq(
        ProcessCommodities(Seq((Fixtures.Resource.properties.commodity, CommodityAmount(-42)))),
        ProcessCommodities(Seq((Fixtures.Resource.properties.commodity, CommodityAmount(-1))))
      )
    )

    val expectedFinalAmount =
      (Fixtures.Resource.state.currentAmount
        + Fixtures.Resource.state.replenishAmount
        - CommodityAmount(42)
        - CommodityAmount(1))

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Fixtures.Resource.properties.commodity,
            Fixtures.Resource.state.replenishAmount,
            CommodityState.Produced
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(
            Fixtures.Resource.properties.commodity,
            expectedFinalAmount,
            Fixtures.Resource.properties.id
          )
        )
      )
    )

    expectMsg(
      Fixtures.Resource.state.copy(expectedFinalAmount)
    )
  }
}
