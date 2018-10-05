package owe.test.specs.unit.entities.active.behaviour.resource

import scala.concurrent.duration._

import akka.actor.Props
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.ResourceData
import owe.entities.ActiveEntityActor._
import owe.entities.Entity.ProcessCommodities
import owe.entities.active.behaviour.resource.producing.ProducingResource
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Commodity
import owe.production.Exchange.{AddProducer, CommodityAvailable, RemoveProducer}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.ResourceParentEntity.MockDestroySelf
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, ResourceParentEntity}

class BaseResourceSpec extends AkkaUnitSpec("BaseResourceSpec") {

  private implicit val timeout: Timeout = 3.seconds

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A BaseResource" should "register with exchange as producer" in { _ =>
    val parentEntity = system.actorOf(
      ResourceParentEntity.props(
        testActor,
        Props(
          new ProducingResource {}
        )
      )
    )

    val resourceData = ResourceData(
      properties = Fixtures.Resource.properties,
      state = Fixtures.Resource.state,
      modifiers = Fixtures.Resource.modifiers,
      Fixtures.MockRefs.resource
    )

    parentEntity ! CreateBehaviour(resourceData)

    val expectedExchangeMessage = AddProducer(resourceData.id, resourceData.properties.commodity)

    val actualExchangeMessage = receiveOne(timeout.duration) match {
      case ForwardMessage(ForwardExchangeMessage(message: AddProducer)) =>
        message.copy(source = resourceData.id)

      case message =>
        fail(s"Unexpected message encountered: [$message]")
    }

    actualExchangeMessage should be(expectedExchangeMessage)
  }

  it should "have destroying behaviour" in { _ =>
    val parentEntity = system.actorOf(
      ResourceParentEntity.props(
        testActor,
        Props(
          new ProducingResource {}
        )
      )
    )

    val resourceData = ResourceData(
      properties = Fixtures.Resource.properties,
      state = Fixtures.Resource.state,
      modifiers = Fixtures.Resource.modifiers,
      Fixtures.MockRefs.resource
    )

    parentEntity ! MockDestroySelf(resourceData)

    val expectedExchangeMessageForCommodities =
      CommodityAvailable(Commodity("TestCommodity"), Commodity.Amount(0), resourceData.id)

    val actualExchangeMessageForCommodities = receiveOne(timeout.duration) match {
      case ForwardMessage(ForwardExchangeMessage(message: CommodityAvailable)) =>
        message.copy(source = resourceData.id)

      case message =>
        fail(s"Unexpected message encountered: [$message]")
    }

    actualExchangeMessageForCommodities should be(expectedExchangeMessageForCommodities)

    val expectedExchangeMessageForProducers =
      RemoveProducer(resourceData.id, Commodity("TestCommodity"))

    val actualExchangeMessageForProducers = receiveOne(timeout.duration) match {
      case ForwardMessage(ForwardExchangeMessage(message: RemoveProducer)) =>
        message.copy(source = resourceData.id)

      case message =>
        fail(s"Unexpected message encountered: [$message]")
    }

    actualExchangeMessageForProducers should be(expectedExchangeMessageForProducers)

    expectMsg(
      BehaviourDestroyed()
    )

    // should ignore instructions
    parentEntity ! ApplyInstructions(resourceData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should ignore messages
    parentEntity ! ApplyMessages(
      entity = resourceData,
      messages = Seq(
        ProcessCommodities(Seq((Fixtures.Resource.properties.commodity, Commodity.Amount(-10))))
      )
    )

    expectMsg(
      MessagesApplied(
        resourceData.state
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = resourceData
    )

    // no state update expected when destroying
    expectMsg(
      BehaviourTickProcessed(
        resourceData.state
      )
    )
  }
}
