package owe.test.specs.unit.entities.active.behaviour.structure.producing

import scala.concurrent.duration._

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor._
import owe.entities.Entity.ProcessCommodities
import owe.entities.active.Structure._
import owe.entities.active.attributes.Life
import owe.entities.active.behaviour.structure.producing.ProducingStructure
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Commodity
import owe.production.Exchange.{CommodityAvailable, CommodityRequired, UpdateCommodityState}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, ForwardingParentEntity}

class ProducingStructureSpec extends AkkaUnitSpec("ProducingStructureSpec") {

  private implicit val timeout: Timeout = 5.seconds

  case class FixtureParam(structure: StructureData, parentEntity: ActorRef)

  def withFixture(test: OneArgTest): Outcome = {
    val structure = StructureData(
      properties = Fixtures.Structure.Producing.properties,
      state = State(
        risk = NoRisk,
        commodities = CommoditiesState(
          available = Map(
            Commodity("TestCommodity#2") -> Commodity.Amount(70)
          ),
          limits = Map(
            Commodity("TestCommodity#1") -> Commodity.Amount(50),
            Commodity("TestCommodity#2") -> Commodity.Amount(100)
          )
        ),
        housing = NoHousing,
        production = ProductionState(
          employees = 15,
          labour = LabourState.Found,
          rates = Map(Commodity("TestCommodity#1") -> Commodity.Amount(25))
        ),
        currentStage = DefaultStage,
        currentLife = Life(100),
        walkers = NoWalkers
      ),
      modifiers = StateModifiers(
        risk = NoRisk,
        commodities = CommoditiesModifier(
          usageRates = Map(
            Commodity("TestCommodity#2") -> Commodity.Amount(15)
          )
        ),
        production = ProductionModifier(
          rates = Map(
            Commodity("TestCommodity#1") -> Commodity.AmountModifier(100)
          )
        ),
        housing = NoHousing
      ),
      Fixtures.MockRefs.structure
    )

    val parentEntity = system.actorOf(ForwardingParentEntity.props(testActor, Props(new ProducingStructure {})))

    withFixture(test.toNoArgTest(FixtureParam(structure, parentEntity)))
  }

  "A Producing structure" should "process instructions and messages" in { fixture =>
    val structureData = StructureData(
      Fixtures.Structure.Housing.properties,
      Fixtures.Structure.Housing.state,
      Fixtures.Structure.Housing.modifiers,
      Fixtures.MockRefs.structure
    )

    // should process instructions
    fixture.parentEntity ! ApplyInstructions(structureData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should process messages
    fixture.parentEntity ! ApplyMessages(
      entity = structureData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity#1"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Structure.Housing.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity#1") -> Commodity.Amount(10)),
            limits = Map(
              Commodity("TestCommodity#1") -> Commodity.Amount(100),
              Commodity("TestCommodity#2") -> Commodity.Amount(10),
              Commodity("TestCommodity#3") -> Commodity.Amount(200)
            )
          )
        )
      )
    )
  }

  it should "generate walkers" in { fixture =>
    fixture.parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = fixture.structure
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity#1"),
            Commodity.Amount(25),
            Commodity.State.Produced
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity#2"),
            Commodity.Amount(15),
            Commodity.State.Used
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityRequired(
            Commodity("TestCommodity#2"),
            Commodity.Amount(45),
            Fixtures.MockRefs.structure
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(
            Commodity("TestCommodity#2"),
            Commodity.Amount(55),
            Fixtures.MockRefs.structure
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(
            Commodity("TestCommodity#1"),
            Commodity.Amount(25),
            Fixtures.MockRefs.structure
          )
        )
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        fixture.structure.state.copy(
          currentLife = Life(100),
          commodities = CommoditiesState(
            available = Map(
              Commodity("TestCommodity#1") -> Commodity.Amount(25),
              Commodity("TestCommodity#2") -> Commodity.Amount(55)
            ),
            limits = Map(
              Commodity("TestCommodity#1") -> Commodity.Amount(50),
              Commodity("TestCommodity#2") -> Commodity.Amount(100)
            )
          )
        )
      )
    )
  }
}
