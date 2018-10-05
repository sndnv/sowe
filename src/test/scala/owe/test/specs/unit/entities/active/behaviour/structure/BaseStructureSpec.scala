package owe.test.specs.unit.entities.active.behaviour.structure

import scala.concurrent.duration._
import akka.actor.Props
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor._
import owe.entities.Entity.ProcessCommodities
import owe.entities.active.Structure
import owe.entities.active.Structure.{CommoditiesModifier, CommoditiesState}
import owe.entities.active.attributes.Life
import owe.entities.active.behaviour.structure.producing.ProducingStructure
import owe.map.GameMap.{DestroyEntity, ForwardExchangeMessage}
import owe.production.Commodity
import owe.production.Exchange._
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.StructureParentEntity.MockDestroySelf
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, StructureParentEntity}

class BaseStructureSpec extends AkkaUnitSpec("BaseStructureSpec") {

  private implicit val timeout: Timeout = 3.seconds

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A BaseStructure" should "register with exchange as producer and consumer" in { _ =>
    val parentEntity = system.actorOf(
      StructureParentEntity.props(
        testActor,
        Props(
          new ProducingStructure {}
        )
      )
    )

    val structureData = StructureData(
      properties = Fixtures.Structure.Producing.properties,
      state = Fixtures.Structure.Producing.state.copy(
        risk = Structure.NoRisk,
        currentLife = Life(0),
        commodities = CommoditiesState(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(200)),
          limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
        )
      ),
      modifiers = Fixtures.Structure.Producing.modifiers.copy(
        risk = Structure.NoRisk,
        commodities = CommoditiesModifier(usageRates = Map(Commodity("TestCommodity") -> Commodity.Amount(5)))
      ),
      Fixtures.MockRefs.structure
    )

    parentEntity ! CreateBehaviour(structureData)

    val expectedExchangeMessageForProducers = AddProducer(structureData.id, Commodity("TestCommodity"))

    val actualExchangeMessageForProducers = receiveOne(timeout.duration) match {
      case ForwardMessage(ForwardExchangeMessage(message: AddProducer)) =>
        message.copy(source = structureData.id)

      case message =>
        fail(s"Unexpected message encountered: [$message]")
    }

    actualExchangeMessageForProducers should be(expectedExchangeMessageForProducers)

    val expectedExchangeMessageForConsumers = AddConsumer(structureData.id, Commodity("TestCommodity"))

    val actualExchangeMessageForConsumers = receiveOne(timeout.duration) match {
      case ForwardMessage(ForwardExchangeMessage(message: AddConsumer)) =>
        message.copy(source = structureData.id)

      case message =>
        fail(s"Unexpected message encountered: [$message]")
    }

    actualExchangeMessageForConsumers should be(expectedExchangeMessageForConsumers)
  }

  it should "destroy itself when it has insufficient life" in { _ =>
    val parentEntity = system.actorOf(
      StructureParentEntity.props(
        testActor,
        Props(
          new ProducingStructure {}
        )
      )
    )

    val structureData = StructureData(
      properties = Fixtures.Structure.Producing.properties,
      state = Fixtures.Structure.Producing.state.copy(
        risk = Structure.NoRisk,
        currentLife = Life(0),
        commodities = CommoditiesState(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(200)),
          limits = Map.empty
        )
      ),
      modifiers = Fixtures.Structure.Producing.modifiers.copy(
        risk = Structure.NoRisk
      ),
      Fixtures.MockRefs.structure
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = structureData
    )

    // no state update expected when destroying
    expectMsg(
      BehaviourTickProcessed(
        structureData.state
      )
    )

    expectMsg(
      ForwardMessage(DestroyEntity(structureData.id))
    )

    parentEntity ! MockDestroySelf(structureData)

    val expectedExchangeMessageForProducers = RemoveProducer(structureData.id, Commodity("TestCommodity"))

    val actualExchangeMessageForProducers = receiveOne(timeout.duration) match {
      case ForwardMessage(ForwardExchangeMessage(message: RemoveProducer)) =>
        message.copy(source = structureData.id)

      case message =>
        fail(s"Unexpected message encountered: [$message]")
    }

    actualExchangeMessageForProducers should be(expectedExchangeMessageForProducers)

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity"),
            Commodity.Amount(200),
            Commodity.State.Lost
          )
        )
      )
    )

    val expectedExchangeMessage = CommodityAvailable(Commodity("TestCommodity"), Commodity.Amount(0), structureData.id)

    val actualExchangeMessage = receiveOne(timeout.duration) match {
      case ForwardMessage(ForwardExchangeMessage(message: CommodityAvailable)) =>
        message.copy(source = structureData.id)

      case message =>
        fail(s"Unexpected message encountered: [$message]")
    }

    actualExchangeMessage should be(expectedExchangeMessage)

    expectMsg(
      BehaviourDestroyed()
    )

    // should ignore instructions
    parentEntity ! ApplyInstructions(structureData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should ignore messages
    parentEntity ! ApplyMessages(
      entity = structureData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        structureData.state
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = structureData
    )

    // no state update expected when destroying
    expectMsg(
      BehaviourTickProcessed(
        structureData.state
      )
    )
  }
}
