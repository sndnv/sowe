package owe.test.specs.unit.entities.active.behaviour.structure.farming

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity.{ForwardMessage, ProcessEntityTick, StructureData}
import owe.entities.active.RiskAmount
import owe.entities.active.Structure.{CommoditiesState, RiskState}
import owe.entities.active.behaviour.structure.farming.FarmingStructure
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Commodity
import owe.production.Exchange.{CommodityAvailable, UpdateCommodityState}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, ForwardingParentEntity}

import scala.concurrent.duration._

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
    fixture.parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = StructureData(
        Fixtures.Structure.Producing.properties,
        Fixtures.Structure.Producing.state,
        Fixtures.Structure.Producing.modifiers,
        Fixtures.MockRefs.structure
      ),
      messages = Seq.empty
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
      Fixtures.Structure.Producing.state
        .copy(
          risk = RiskState(fire = RiskAmount(3), damage = RiskAmount(5)),
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(25))
          )
        )
    )
  }
}
