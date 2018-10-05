package owe.test.specs.unit.production

import scala.concurrent.duration._

import akka.actor.ActorRef
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.production.Exchange._
import owe.production.{Commodity, Exchange}
import owe.test.specs.unit.AkkaUnitSpec

class ExchangeSpec extends AkkaUnitSpec("ExchangeSpec") {

  private implicit val timeout: Timeout = 3.seconds

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  private val exchange: ActorRef = system.actorOf(Exchange.props())
  private val testProducer1 = (StructureRef(TestProbe().ref), Commodity("TestCommodity#1"))
  private val testProducer2 = (StructureRef(TestProbe().ref), Commodity("TestCommodity#1"))
  private val testConsumer1 = (StructureRef(TestProbe().ref), Commodity("TestCommodity#1"))
  private val testConsumer2 = (StructureRef(TestProbe().ref), Commodity("TestCommodity#3"))
  private val testConsumer3 = (StructureRef(TestProbe().ref), Commodity("TestCommodity#3"))
  private val testCarrier1 = (WalkerRef(TestProbe().ref), Commodity("TestCommodity#3"))
  private val testCarrier2 = (WalkerRef(TestProbe().ref), Commodity("TestCommodity#4"))

  "An Exchange" should "add and remove producers and consumers" in { _ =>
    exchange ! AddProducer(testProducer1._1, testProducer1._2)
    exchange ! AddProducer(testProducer2._1, testProducer2._2)
    exchange ! AddConsumer(testConsumer1._1, testConsumer1._2)
    exchange ! AddConsumer(testConsumer2._1, testConsumer2._2)
    exchange ! AddConsumer(testConsumer3._1, testConsumer3._2)
    exchange ! GetExchangeEntities()

    receiveOne(timeout.duration).asInstanceOf[ExchangeEntities] should be(
      ExchangeEntities.empty.copy(
        producers = Map(
          testProducer1._2 -> Seq(testProducer1._1, testProducer2._1)
        ),
        consumers = Map(
          testConsumer1._2 -> Seq(testConsumer1._1),
          testConsumer2._2 -> Seq(testConsumer2._1, testConsumer3._1)
        )
      )
    )

    exchange ! RemoveProducer(testProducer1._1, testProducer1._2)
    exchange ! RemoveProducer(testProducer2._1, testProducer2._2)
    exchange ! RemoveConsumer(testConsumer1._1, testConsumer1._2)
    exchange ! RemoveConsumer(testConsumer3._1, testConsumer3._2)
    exchange ! GetExchangeEntities()

    receiveOne(timeout.duration).asInstanceOf[ExchangeEntities] should be(
      ExchangeEntities.empty.copy(
        consumers = Map(
          testConsumer2._2 -> Seq(testConsumer2._1)
        )
      )
    )
  }

  it should "accept commodity state updates" in { _ =>
    exchange ! CommodityRequired(Commodity("TestCommodity#2"), Commodity.Amount(10), testProducer1._1)
    exchange ! CommodityRequired(Commodity("TestCommodity#3"), Commodity.Amount(15), testProducer1._1)

    val stateWithRequiredCommodities = ExchangeCommodities.empty.copy(
      required = Map(
        (Commodity("TestCommodity#2"), testProducer1._1) -> Commodity.Amount(10),
        (Commodity("TestCommodity#3"), testProducer1._1) -> Commodity.Amount(15)
      )
    )

    exchange ! GetExchangeCommodities()
    receiveOne(timeout.duration).asInstanceOf[ExchangeCommodities] should be(stateWithRequiredCommodities)

    exchange ! CommodityAvailable(Commodity("TestCommodity#1"), Commodity.Amount(25), testProducer1._1)
    exchange ! CommodityAvailable(Commodity("TestCommodity#1"), Commodity.Amount(50), testProducer1._1)

    val stateWithAvailableCommodities = stateWithRequiredCommodities.copy(
      available = Map(
        (Commodity("TestCommodity#1"), testProducer1._1) -> Commodity.Amount(50)
      )
    )

    exchange ! GetExchangeCommodities()
    receiveOne(timeout.duration).asInstanceOf[ExchangeCommodities] should be(stateWithAvailableCommodities)

    exchange ! CommodityInTransit(testCarrier1._2, Commodity.Amount(5), testCarrier1._1, testProducer1._1)

    val stateWithInTransitCommodities = stateWithAvailableCommodities.copy(
      inTransit = Map(
        (testCarrier1._2, testCarrier1._1) -> (Commodity.Amount(5), testProducer1._1)
      )
    )

    exchange ! GetExchangeCommodities()
    receiveOne(timeout.duration).asInstanceOf[ExchangeCommodities] should be(stateWithInTransitCommodities)
  }

  it should "be able to reset commodity stats" in { _ =>
    exchange ! CommodityRequired(Commodity("TestCommodity#2"), Commodity.Amount(10), testProducer1._1)
    exchange ! CommodityRequired(Commodity("TestCommodity#3"), Commodity.Amount(15), testProducer1._1)
    exchange ! CommodityAvailable(Commodity("TestCommodity#1"), Commodity.Amount(25), testProducer1._1)
    exchange ! CommodityAvailable(Commodity("TestCommodity#1"), Commodity.Amount(50), testProducer1._1)
    exchange ! CommodityInTransit(testCarrier1._2, Commodity.Amount(5), testCarrier1._1, testProducer1._1)
    exchange ! CommodityInTransit(testCarrier2._2, Commodity.Amount(5), testCarrier2._1, testProducer2._1)

    val expectedCommodities = ExchangeCommodities.empty.copy(
      required = Map(
        (Commodity("TestCommodity#2"), testProducer1._1) -> Commodity.Amount(10),
        (Commodity("TestCommodity#3"), testProducer1._1) -> Commodity.Amount(15)
      ),
      available = Map(
        (Commodity("TestCommodity#1"), testProducer1._1) -> Commodity.Amount(50)
      ),
      inTransit = Map(
        (testCarrier1._2, testCarrier1._1) -> (Commodity.Amount(5), testProducer1._1),
        (testCarrier2._2, testCarrier2._1) -> (Commodity.Amount(5), testProducer2._1)
      )
    )

    exchange ! GetExchangeCommodities()
    val actualCommodities = receiveOne(timeout.duration).asInstanceOf[ExchangeCommodities]
    actualCommodities should be(expectedCommodities)

    exchange ! CommodityRequired(Commodity("TestCommodity#2"), Commodity.Amount(0), testProducer1._1)
    val expectedCommoditiesWithRequiredReset = expectedCommodities.copy(
      required = Map(
        (Commodity("TestCommodity#3"), testProducer1._1) -> Commodity.Amount(15)
      )
    )
    exchange ! GetExchangeCommodities()
    val actualCommoditiesWithRequiredReset = receiveOne(timeout.duration).asInstanceOf[ExchangeCommodities]
    actualCommoditiesWithRequiredReset should be(expectedCommoditiesWithRequiredReset)

    exchange ! CommodityAvailable(Commodity("TestCommodity#1"), Commodity.Amount(0), testProducer1._1)
    val expectedCommoditiesWithAvailableReset = expectedCommoditiesWithRequiredReset.copy(
      available = Map.empty
    )
    exchange ! GetExchangeCommodities()
    val actualCommoditiesWithAvailableReset = receiveOne(timeout.duration).asInstanceOf[ExchangeCommodities]
    actualCommoditiesWithAvailableReset should be(expectedCommoditiesWithAvailableReset)

    exchange ! CommodityInTransit(testCarrier1._2, Commodity.Amount(0), testCarrier1._1, testProducer1._1)
    val expectedCommoditiesWithInTransitReset = expectedCommoditiesWithAvailableReset.copy(
      inTransit = Map(
        (testCarrier2._2, testCarrier2._1) -> (Commodity.Amount(5), testProducer2._1)
      )
    )
    exchange ! GetExchangeCommodities()
    val actualCommoditiesWithInTransitReset = receiveOne(timeout.duration).asInstanceOf[ExchangeCommodities]
    actualCommoditiesWithInTransitReset should be(expectedCommoditiesWithInTransitReset)

    exchange ! CommodityNotInTransit(testCarrier2._2, testCarrier2._1)
    val expectedCommoditiesWithNotInTransit = expectedCommoditiesWithInTransitReset.copy(
      inTransit = Map.empty
    )
    exchange ! GetExchangeCommodities()
    val actualCommoditiesWithNotInTransit = receiveOne(timeout.duration).asInstanceOf[ExchangeCommodities]
    actualCommoditiesWithNotInTransit should be(expectedCommoditiesWithNotInTransit)
  }

  it should "accept stats updates" in { _ =>
    exchange ! UpdateCommodityState(Commodity("TestCommodity#1"), Commodity.Amount(25), Commodity.State.Produced)
    exchange ! UpdateCommodityState(Commodity("TestCommodity#1"), Commodity.Amount(50), Commodity.State.Produced)
    exchange ! UpdateCommodityState(Commodity("TestCommodity#2"), Commodity.Amount(10), Commodity.State.Used)
    exchange ! UpdateCommodityState(Commodity("TestCommodity#3"), Commodity.Amount(15), Commodity.State.Used)
    exchange ! UpdateCommodityState(Commodity("TestCommodity#3"), Commodity.Amount(100), Commodity.State.Lost)
    exchange ! GetExchangeStats()

    val stateWithUpdates = ExchangeStats.empty.copy(
      produced = Map(
        Commodity("TestCommodity#1") -> Commodity.Amount(75)
      ),
      used = Map(
        Commodity("TestCommodity#2") -> Commodity.Amount(10),
        Commodity("TestCommodity#3") -> Commodity.Amount(15)
      ),
      lost = Map(
        Commodity("TestCommodity#3") -> Commodity.Amount(100)
      )
    )

    val actualStateWithUpdates = receiveOne(timeout.duration).asInstanceOf[ExchangeStats]
    actualStateWithUpdates should be(stateWithUpdates)
  }
}
