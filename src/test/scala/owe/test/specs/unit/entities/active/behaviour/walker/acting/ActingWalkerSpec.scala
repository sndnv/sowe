package owe.test.specs.unit.entities.active.behaviour.walker.acting

import akka.actor.Props
import akka.pattern.pipe
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity.WalkerData
import owe.entities.ActiveEntityActor.ForwardMessage
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker._
import owe.entities.active.attributes.{Life, Speed}
import owe.entities.active.behaviour.walker.acting.ActingWalker
import owe.map.GameMap.{DistributeCommodities, ForwardExchangeMessage}
import owe.map.grid.Point
import owe.production.Commodity
import owe.production.Exchange.{AddConsumer, AddProducer, CommodityInTransit}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.WalkerParentEntity.ForwardToChild
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, WalkerParentEntity}

import scala.concurrent.duration._

class ActingWalkerSpec extends AkkaUnitSpec("ActingWalkerSpec") {

  private case class Gather(walker: WalkerData, source: ResourceRef, target: StructureRef)
  private case class Load(walker: WalkerData, source: StructureRef, target: StructureRef)
  private case class Unload(walker: WalkerData, target: StructureRef)

  private class TestWalker extends ActingWalker {
    import system.dispatcher

    override protected def behaviour: Behaviour = {
      case Gather(walker, source, target) => gather(walker, source, target).pipeTo(sender)
      case Load(walker, source, target)   => load(walker, source, target).pipeTo(sender)
      case Unload(walker, target)         => unload(walker, target).pipeTo(sender)
    }
  }

  private implicit val timeout: Timeout = 5.seconds

  private val properties: Properties = Fixtures.Walker.properties.copy(
    homePosition = Point(0, 0),
    name = "ActingWalker",
    movementSpeed = Speed(1)
  )

  private val state: State = Fixtures.Walker.state.copy(
    currentLife = Life(50),
    commodities = CommoditiesState(
      available = Map.empty,
      limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
    ),
    mode = MovementMode.Roaming
  )

  private val modifiers: StateModifiers = Fixtures.Walker.modifiers

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An Acting walker" should "gather resources" in { _ =>
    val testProbe = TestProbe()
    testProbe.ignoreMsg {
      case _: AddProducer => true
      case _: AddConsumer => true
    }

    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testProbe.ref,
        Props(new TestWalker)
      )
    )

    val walker = WalkerData(
      properties,
      state,
      modifiers,
      Fixtures.MockRefs.walker
    )

    parentEntity.tell(
      ForwardToChild(
        Gather(
          walker,
          Fixtures.MockRefs.resource,
          Fixtures.MockRefs.structure
        )
      ),
      testProbe.ref
    )

    val successfulResult = testProbe.receiveWhile(timeout.duration) {
      case ForwardMessage(DistributeCommodities(_, commodities)) =>
        commodities should be(Seq((Commodity("TestCommodity"), Commodity.Amount(-100))))
        true

      case ForwardMessage(ForwardExchangeMessage(message)) =>
        message match {
          case CommodityInTransit(commodity, amount, _, _) =>
            commodity should be(Commodity("TestCommodity"))
            amount should be(Commodity.Amount(100))

          case _ =>
            fail(s"Unexpected message received: [$message]")
        }
        true

      case state: State =>
        state should be(
          walker.state.copy(
            commodities = CommoditiesState(
              available = Map(Commodity("TestCommodity") -> Commodity.Amount(100)),
              limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
            )
          )
        )
        true
    }

    successfulResult should be(Seq(true, true, true))

    val stateWithoutCommodities = walker.state.copy(commodities = NoCommodities)

    parentEntity.tell(
      ForwardToChild(
        Gather(
          walker.copy(state = stateWithoutCommodities),
          Fixtures.MockRefs.resource,
          Fixtures.MockRefs.structure
        )
      ),
      testProbe.ref
    )

    testProbe.receiveOne(timeout.duration) match {
      case state: State => state should be(stateWithoutCommodities)
      case message      => fail(s"Unexpected message received: [$message]")
    }
  }

  it should "load commodities" in { _ =>
    val testProbe = TestProbe()
    testProbe.ignoreMsg {
      case _: AddProducer => true
      case _: AddConsumer => true
    }

    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testProbe.ref,
        Props(new TestWalker)
      )
    )

    val walker = WalkerData(
      properties,
      state,
      modifiers,
      Fixtures.MockRefs.walker
    )

    parentEntity.tell(
      ForwardToChild(
        Load(
          walker,
          Fixtures.MockRefs.structure,
          Fixtures.MockRefs.structure
        )
      ),
      testProbe.ref
    )

    val successfulResult = testProbe.receiveWhile(timeout.duration) {
      case ForwardMessage(DistributeCommodities(_, commodities)) =>
        commodities should be(Seq((Commodity("TestCommodity"), Commodity.Amount(-100))))
        true

      case ForwardMessage(ForwardExchangeMessage(message)) =>
        message match {
          case CommodityInTransit(commodity, amount, _, _) =>
            commodity should be(Commodity("TestCommodity"))
            amount should be(Commodity.Amount(100))

          case _ =>
            fail(s"Unexpected message received: [$message]")
        }
        true

      case state: State =>
        state should be(
          walker.state.copy(
            commodities = CommoditiesState(
              available = Map(Commodity("TestCommodity") -> Commodity.Amount(100)),
              limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
            )
          )
        )
        true
    }

    successfulResult should be(Seq(true, true, true))

    val stateWithoutCommodities = walker.state.copy(commodities = NoCommodities)

    parentEntity.tell(
      ForwardToChild(
        Load(
          walker.copy(state = stateWithoutCommodities),
          Fixtures.MockRefs.structure,
          Fixtures.MockRefs.structure
        )
      ),
      testProbe.ref
    )

    testProbe.receiveOne(timeout.duration) match {
      case state: State => state should be(stateWithoutCommodities)
      case message      => fail(s"Unexpected message received: [$message]")
    }
  }

  it should "unload commodities" in { _ =>
    val testProbe = TestProbe()
    testProbe.ignoreMsg {
      case _: AddProducer => true
      case _: AddConsumer => true
    }

    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testProbe.ref,
        Props(new TestWalker)
      )
    )

    val walker = WalkerData(
      properties,
      state.copy(
        commodities = CommoditiesState(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(100)),
          limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
        )
      ),
      modifiers,
      Fixtures.MockRefs.walker
    )

    parentEntity.tell(
      ForwardToChild(
        Unload(
          walker,
          Fixtures.MockRefs.structure
        )
      ),
      testProbe.ref
    )

    val successfulResult = testProbe.receiveWhile(timeout.duration) {
      case ForwardMessage(DistributeCommodities(_, commodities)) =>
        commodities should be(Seq((Commodity("TestCommodity"), Commodity.Amount(75))))
        true

      case ForwardMessage(ForwardExchangeMessage(message)) =>
        message match {
          case CommodityInTransit(commodity, amount, _, _) =>
            commodity should be(Commodity("TestCommodity"))
            amount should be(Commodity.Amount(25))

          case _ =>
            fail(s"Unexpected message received: [$message]")
        }
        true

      case state: State =>
        state should be(
          walker.state.copy(
            commodities = CommoditiesState(
              available = Map(Commodity("TestCommodity") -> Commodity.Amount(25)),
              limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
            )
          )
        )
        true
    }

    successfulResult should be(Seq(true, true, true))

    val stateWithoutCommodities = walker.state.copy(commodities = NoCommodities)

    parentEntity.tell(
      ForwardToChild(
        Unload(
          walker.copy(state = stateWithoutCommodities),
          Fixtures.MockRefs.structure
        )
      ),
      testProbe.ref
    )

    testProbe.receiveOne(timeout.duration) match {
      case state: State => state should be(stateWithoutCommodities)
      case message      => fail(s"Unexpected message received: [$message]")
    }
  }
}
