package owe.test.specs.unit.entities.active.behaviour.resource.producing

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity.ResourceData
import owe.entities.ActiveEntityActor._
import owe.entities.Entity.ProcessCommodities
import owe.entities.active.Resource
import owe.entities.active.behaviour.resource.producing.ProducingResource
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Commodity
import owe.production.Exchange.{CommodityAvailable, UpdateCommodityState}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, ForwardingParentEntity}

import scala.concurrent.duration._

class ProducingResourceSpec extends AkkaUnitSpec("ProducingResourceSpec") {

  private implicit val timeout: Timeout = 5.seconds

  case class FixtureParam(parentEntity: ActorRef)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(
      test.toNoArgTest(
        FixtureParam(
          parentEntity = system.actorOf(ForwardingParentEntity.props(testActor, Props(new ProducingResource {}))))
      )
    )

  "A ProducingResource" should "produce commodities" in { fixture =>
    fixture.parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = ResourceData(
        Fixtures.Resource.properties,
        Fixtures.Resource.state,
        Fixtures.Resource.modifiers,
        Fixtures.MockRefs.resource
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Fixtures.Resource.properties.commodity,
            Fixtures.Resource.state.replenishAmount,
            Commodity.State.Produced
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
            Fixtures.MockRefs.resource
          )
        )
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Resource.state
          .copy(currentAmount = Fixtures.Resource.state.currentAmount + Fixtures.Resource.state.replenishAmount)
      )
    )
  }

  it should "allow commodities to be retrieved by walkers" in { fixture =>
    fixture.parentEntity ! ApplyMessages(
      ResourceData(
        Fixtures.Resource.properties,
        Fixtures.Resource.state,
        Fixtures.Resource.modifiers,
        Fixtures.MockRefs.resource
      ),
      messages = Seq(
        ProcessCommodities(Seq((Fixtures.Resource.properties.commodity, Commodity.Amount(-42)))),
        ProcessCommodities(Seq((Fixtures.Resource.properties.commodity, Commodity.Amount(-1))))
      )
    )

    val updatedState = receiveOne(timeout.duration).asInstanceOf[MessagesApplied[Resource.State]].state

    fixture.parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = ResourceData(
        Fixtures.Resource.properties,
        updatedState,
        Fixtures.Resource.modifiers,
        Fixtures.MockRefs.resource
      )
    )

    val expectedFinalAmount =
      (Fixtures.Resource.state.currentAmount
        + Fixtures.Resource.state.replenishAmount
        - Commodity.Amount(42)
        - Commodity.Amount(1))

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Fixtures.Resource.properties.commodity,
            Fixtures.Resource.state.replenishAmount,
            Commodity.State.Produced
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
            Fixtures.MockRefs.resource
          )
        )
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Resource.state.copy(expectedFinalAmount)
      )
    )
  }
}
