package owe.test.specs.unit.entities.active.behaviour.structure.housing

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor._
import owe.entities.active.Structure._
import owe.entities.active._
import owe.entities.active.attributes.RiskAmount
import owe.entities.active.behaviour.structure.housing.HousingStructure
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Commodity
import owe.production.Exchange.{CommodityRequired, UpdateCommodityState}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, ForwardingParentEntity}
import scala.concurrent.duration._

import owe.entities.ActiveEntity
import owe.entities.Entity.ProcessCommodities

class HousingStructureSpec extends AkkaUnitSpec("HousingStructureSpec") {

  private implicit val timeout: Timeout = 5.seconds

  case class FixtureParam(parentEntity: ActorRef)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(
      test.toNoArgTest(
        FixtureParam(
          parentEntity = system.actorOf(ForwardingParentEntity.props(testActor, Props(new HousingStructure {}))))
      )
    )

  "A Housing structure" should "consume and require commodities" in { fixture =>
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

    fixture.parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = StructureData(
        Fixtures.Structure.Housing.properties,
        Fixtures.Structure.Housing.state.copy(
          housing = Fixtures.Structure.Housing.state.housing
            .asInstanceOf[HousingState]
            .copy(
              occupants = 1
            ),
          commodities = CommoditiesState(
            available = Map(
              Commodity("TestCommodity#1") -> Commodity.Amount(25),
              Commodity("TestCommodity#2") -> Commodity.Amount(5),
              Commodity("TestCommodity#3") -> Commodity.Amount(25)
            ),
            limits = Map(
              Commodity("TestCommodity#1") -> Commodity.Amount(100),
              Commodity("TestCommodity#2") -> Commodity.Amount(10),
              Commodity("TestCommodity#3") -> Commodity.Amount(200)
            )
          )
        ),
        Fixtures.Structure.Housing.modifiers,
        Fixtures.MockRefs.structure
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity#1"),
            Commodity.Amount(5),
            Commodity.State.Used
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity#2"),
            Commodity.Amount(1),
            Commodity.State.Used
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity#3"),
            Commodity.Amount(5),
            Commodity.State.Used
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityRequired(
            Commodity("TestCommodity#1"),
            Commodity.Amount(80),
            Fixtures.MockRefs.structure
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityRequired(
            Commodity("TestCommodity#2"),
            Commodity.Amount(6),
            Fixtures.MockRefs.structure
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityRequired(
            Commodity("TestCommodity#3"),
            Commodity.Amount(180),
            Fixtures.MockRefs.structure
          )
        )
      )
    )

    val commoditiesState = Fixtures.Structure.Housing.state.commodities.asInstanceOf[CommoditiesState]

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Structure.Housing.state.copy(
          risk = RiskState(fire = RiskAmount(3), damage = RiskAmount(5)),
          commodities = commoditiesState.copy(
            available = Map(
              Commodity("TestCommodity#1") -> Commodity.Amount(20),
              Commodity("TestCommodity#2") -> Commodity.Amount(4),
              Commodity("TestCommodity#3") -> Commodity.Amount(20)
            )
          ),
          housing = HousingState(
            occupants = 1,
            commodityShortage = 1,
            education = Map(
              EducationEntry("Entry#1") -> EducationLevel(current = 9, minimal = 0, required = 100)
            ),
            entertainment = Map(
              EntertainmentEntry("Entry#2") -> EntertainmentLevel(current = 19, minimal = 0, required = 100)
            ),
            religion = Map(
              ReligionEntry("Entry#3") -> ReligionLevel(current = 29, minimal = 0, required = 100)
            ),
            healthcare = Map(
              HealthcareEntry("Entry#4") -> HealthcareLevel(current = 39, minimal = 0, required = 100)
            ),
            civilService = Map(
              CivilServiceEntry("Entry#5") -> CivilServiceLevel(current = 49, minimal = 0, required = 100)
            )
          )
        )
      )
    )
  }
}
