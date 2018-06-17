package owe.test.specs.unit.entities.active.behaviour.structure.housing

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity.{ForwardMessage, ProcessEntityTick, StructureData}
import owe.entities.active.Structure._
import owe.entities.active._
import owe.entities.active.behaviour.structure.housing.HousingStructure
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Exchange.{CommodityRequired, UpdateCommodityState}
import owe.production.{Commodity, CommodityAmount, CommodityState}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, TestParentEntity}

import scala.concurrent.duration._

class HousingStructureSpec extends AkkaUnitSpec("HousingStructureSpec") {

  private implicit val timeout: Timeout = 5.seconds

  case class FixtureParam(parentEntity: ActorRef)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(
      test.toNoArgTest(
        FixtureParam(parentEntity = system.actorOf(TestParentEntity.props(testActor, Props(new HousingStructure {}))))
      )
    )

  "A Housing structure" should "consume and require commodities" in { fixture =>
    fixture.parentEntity ! ProcessEntityTick(
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
              Commodity("TestCommodity#1") -> CommodityAmount(25),
              Commodity("TestCommodity#2") -> CommodityAmount(5),
              Commodity("TestCommodity#3") -> CommodityAmount(25)
            ),
            limits = Map(
              Commodity("TestCommodity#1") -> CommodityAmount(100),
              Commodity("TestCommodity#2") -> CommodityAmount(10),
              Commodity("TestCommodity#3") -> CommodityAmount(200)
            )
          )
        ),
        Fixtures.Structure.Housing.modifiers,
        Fixtures.MockRefs.structure
      ),
      messages = Seq.empty
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity#1"),
            CommodityAmount(5),
            CommodityState.Used
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity#2"),
            CommodityAmount(1),
            CommodityState.Used
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity#3"),
            CommodityAmount(5),
            CommodityState.Used
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityRequired(
            Commodity("TestCommodity#1"),
            CommodityAmount(100),
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
            CommodityAmount(10),
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
            CommodityAmount(200),
            Fixtures.MockRefs.structure
          )
        )
      )
    )

    val commoditiesState = Fixtures.Structure.Housing.state.commodities.asInstanceOf[CommoditiesState]

    expectMsg(
      Fixtures.Structure.Housing.state.copy(
        risk = RiskState(fire = RiskAmount(3), damage = RiskAmount(5)),
        commodities = commoditiesState.copy(
          available = Map(
            Commodity("TestCommodity#1") -> CommodityAmount(20),
            Commodity("TestCommodity#2") -> CommodityAmount(4),
            Commodity("TestCommodity#3") -> CommodityAmount(20)
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
  }
}
