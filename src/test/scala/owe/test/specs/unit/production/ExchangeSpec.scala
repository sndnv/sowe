package owe.test.specs.unit.production

import akka.actor.ActorRef
import akka.testkit.TestProbe
import org.scalatest.Outcome
import owe.Tagging._
import owe.entities.Entity
import owe.production.Exchange._
import owe.production.{Commodity, CommodityAmount, CommodityState, Exchange}
import owe.test.specs.unit.AkkaUnitSpec

class ExchangeSpec extends AkkaUnitSpec("ExchangeSpec") {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  private val exchange: ActorRef = system.actorOf(Exchange.props())
  private val testProducer1 = (TestProbe().ref.tag[Entity.ActorRefTag], Commodity("TestCommodity#1"))
  private val testProducer2 = (TestProbe().ref.tag[Entity.ActorRefTag], Commodity("TestCommodity#1"))
  private val testConsumer1 = (TestProbe().ref.tag[Entity.ActorRefTag], Commodity("TestCommodity#1"))
  private val testConsumer2 = (TestProbe().ref.tag[Entity.ActorRefTag], Commodity("TestCommodity#3"))
  private val testCarrier = (TestProbe().ref.tag[Entity.ActorRefTag], Commodity("TestCommodity#3"))

  "An Exchange" should "add and remove producers and consumers" in { _ =>
    exchange ! AddProducer(testProducer1._1, testProducer1._2)
    exchange ! AddProducer(testProducer2._1, testProducer2._2)
    exchange ! AddConsumer(testConsumer1._1, testConsumer1._2)
    exchange ! AddConsumer(testConsumer2._1, testConsumer2._2)
    exchange ! GetExchangeEntities()

    expectMsg(
      ExchangeEntities.empty.copy(
        producers = Map(
          testProducer1._2 -> Seq(testProducer1._1, testProducer2._1)
        ),
        consumers = Map(
          testConsumer1._2 -> Seq(testConsumer1._1),
          testConsumer2._2 -> Seq(testConsumer2._1)
        )
      )
    )

    exchange ! RemoveProducer(testProducer1._1, testProducer1._2)
    exchange ! RemoveProducer(testProducer2._1, testProducer2._2)
    exchange ! RemoveConsumer(testConsumer1._1, testConsumer1._2)
    exchange ! GetExchangeEntities()

    expectMsg(
      ExchangeEntities.empty.copy(
        consumers = Map(
          testConsumer2._2 -> Seq(testConsumer2._1)
        )
      )
    )
  }

  it should "accept commodity updates" in { _ =>
    exchange ! CommodityRequired(Commodity("TestCommodity#2"), CommodityAmount(10), testProducer1._1)
    exchange ! CommodityRequired(Commodity("TestCommodity#3"), CommodityAmount(15), testProducer1._1)
    exchange ! GetExchangeCommodities()

    val stateWithRequiredCommodities = ExchangeCommodities.empty.copy(
      required = Map(
        (Commodity("TestCommodity#2"), testProducer1._1) -> CommodityAmount(10),
        (Commodity("TestCommodity#3"), testProducer1._1) -> CommodityAmount(15)
      )
    )

    expectMsg(stateWithRequiredCommodities)

    exchange ! CommodityAvailable(Commodity("TestCommodity#1"), CommodityAmount(25), testProducer1._1)
    exchange ! CommodityAvailable(Commodity("TestCommodity#1"), CommodityAmount(50), testProducer1._1)
    exchange ! GetExchangeCommodities()

    val stateWithAvailableCommodities = stateWithRequiredCommodities.copy(
      available = Map(
        (Commodity("TestCommodity#1"), testProducer1._1) -> CommodityAmount(50)
      )
    )

    expectMsg(stateWithAvailableCommodities)

    exchange ! CommodityInTransit(testCarrier._2, CommodityAmount(5), testCarrier._1, testProducer1._1)
    exchange ! GetExchangeCommodities()

    val stateWithInTransitCommodities = stateWithAvailableCommodities.copy(
      inTransit = Map(
        (testCarrier._2, testCarrier._1) -> (CommodityAmount(5), testProducer1._1)
      )
    )

    expectMsg(stateWithInTransitCommodities)
  }

  it should "accept stats updates" in { _ =>
    exchange ! UpdateCommodityState(Commodity("TestCommodity#1"), CommodityAmount(25), CommodityState.Produced)
    exchange ! UpdateCommodityState(Commodity("TestCommodity#1"), CommodityAmount(50), CommodityState.Produced)
    exchange ! UpdateCommodityState(Commodity("TestCommodity#2"), CommodityAmount(10), CommodityState.Used)
    exchange ! UpdateCommodityState(Commodity("TestCommodity#3"), CommodityAmount(15), CommodityState.Used)
    exchange ! UpdateCommodityState(Commodity("TestCommodity#3"), CommodityAmount(100), CommodityState.Lost)
    exchange ! GetExchangeStats()

    val stateWithUpdates = ExchangeStats.empty.copy(
      produced = Map(
        Commodity("TestCommodity#1") -> CommodityAmount(75)
      ),
      used = Map(
        Commodity("TestCommodity#2") -> CommodityAmount(10),
        Commodity("TestCommodity#3") -> CommodityAmount(15)
      ),
      lost = Map(
        Commodity("TestCommodity#3") -> CommodityAmount(100)
      )
    )

    expectMsg(stateWithUpdates)
  }
}
