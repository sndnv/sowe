package owe.test.specs.unit.entities.active.behaviour.structure.farming

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor._
import owe.entities.active.Structure.{CommoditiesState, RiskState}
import owe.entities.active.attributes.RiskAmount
import owe.entities.active.behaviour.structure.farming.FarmingStructure
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Commodity
import owe.production.Exchange.{CommodityAvailable, UpdateCommodityState}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, ForwardingParentEntity}
import scala.concurrent.duration._

import owe.entities.ActiveEntity
import owe.entities.Entity.ProcessCommodities

class FarmingStructureSpec extends AkkaUnitSpec("FarmingStructureSpec") {

  private implicit val timeout: Timeout = 5.seconds

  case class FixtureParam(parentEntity: ActorRef)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(
      test.toNoArgTest(
        FixtureParam(
          parentEntity = system.actorOf(ForwardingParentEntity.props(testActor, Props(new FarmingStructure {}))))
      )
    )

  "A Farming structure" should "produce commodities" in { fixture =>
    val structureData = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state,
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    // should process instructions
    fixture.parentEntity ! ApplyInstructions(structureData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should process messages
    fixture.parentEntity ! ApplyMessages(
      entity = structureData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Structure.Producing.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    )

    fixture.parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = structureData
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity"),
            Commodity.Amount(25),
            Commodity.State.Produced
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(
            Commodity("TestCommodity"),
            Commodity.Amount(25),
            Fixtures.MockRefs.structure
          )
        )
      )
    )

    val commoditiesState = Fixtures.Structure.Producing.state.commodities.asInstanceOf[CommoditiesState]

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Structure.Producing.state
          .copy(
            risk = RiskState(fire = RiskAmount(3), damage = RiskAmount(5)),
            commodities = commoditiesState.copy(
              available = Map(Commodity("TestCommodity") -> Commodity.Amount(25))
            )
          )
      )
    )
  }
}
