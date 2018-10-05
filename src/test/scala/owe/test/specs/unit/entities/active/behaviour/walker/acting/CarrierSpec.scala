package owe.test.specs.unit.entities.active.behaviour.walker.acting

import akka.actor.Props
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity._
import owe.entities.Entity
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker._
import owe.entities.active.attributes.{Life, Speed}
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.structure.producing.Industry
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.acting.Carrier
import owe.entities.active.{Structure, Walker}
import owe.events.Event
import owe.map.GameMap.CreateEntity
import owe.map.grid.Point
import owe.production.Commodity
import owe.production.Exchange.{AddConsumer, AddProducer, CommodityInTransit}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures
import owe.test.specs.unit.map.TestGameMap
import owe.test.specs.unit.map.TestGameMap.StartBehaviour

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import owe.events.Event.{CellEvent, EntityEvent, SystemEvent}
import owe.test.specs.unit.entities.EntityTestHelpers

class CarrierSpec extends AkkaUnitSpec("CarrierSpec") with EntityTestHelpers {

  import CarrierSpec.Expectation._
  import CarrierSpec._

  protected implicit val timeout: Timeout = 5.seconds

  private val properties: Properties = Fixtures.Walker.properties.copy(
    homePosition = Point(0, 0),
    name = "Carrier",
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

  "A Carrier walker" should "retrieve commodities and wait at home until free space is available" in { _ =>
    val testProbe = TestProbe()
    testProbe.ignoreMsg {
      case _: AddProducer => true
      case _: AddConsumer => true
    }

    val result = setupGameMapAndCollectEvents(
      testProbe,
      parentStructure = new TestTargetStructure,
      targetStructure = new TestSourceStructure,
      walkerFactory = {
        case (parentStructureRef, sourceStructureRef) =>
          new TestRetrievalCarrier(
            properties.copy(parent = Some(parentStructureRef)),
            state.copy(
              path = Queue.empty
            ),
            modifiers,
            source = sourceStructureRef,
            canReturn = false
          )
      }
    )

    result.ticksProcessed should be > 0
    result.walkerMoved should be > 0
    result.commoditiesDistributed should be(2)
    result.testCommodityRetrieved should be(true)
    result.testCommodityStored should be(true)
    result.testCommodityReturned should be(false)
  }

  it should "retrieve commodities and return remaining to source" in { _ =>
    val testProbe = TestProbe()
    testProbe.ignoreMsg {
      case _: AddProducer => true
      case _: AddConsumer => true
    }

    val result = setupGameMapAndCollectEvents(
      testProbe,
      parentStructure = new TestTargetStructure,
      targetStructure = new TestSourceStructure,
      walkerFactory = {
        case (parentStructureRef, sourceStructureRef) =>
          new TestRetrievalCarrier(
            properties.copy(parent = Some(parentStructureRef)),
            state.copy(
              path = Queue.empty
            ),
            modifiers,
            source = sourceStructureRef,
            canReturn = true
          )
      }
    )

    result.ticksProcessed should be > 0
    result.walkerMoved should be > 0
    result.commoditiesDistributed should be(3)
    result.testCommodityRetrieved should be(true)
    result.testCommodityStored should be(true)
    result.testCommodityReturned should be(true)
  }

  it should "deliver commodities and wait until free space is available" in { _ =>
    val testProbe = TestProbe()
    testProbe.ignoreMsg {
      case _: AddProducer => true
      case _: AddConsumer => true
    }

    val result = setupGameMapAndCollectEvents(
      testProbe,
      parentStructure = new TestSourceStructure,
      targetStructure = new TestTargetStructure,
      walkerFactory = {
        case (parentStructureRef, sourceStructureRef) =>
          new TestDeliveryCarrier(
            properties.copy(parent = Some(parentStructureRef)),
            state.copy(
              path = Queue.empty
            ),
            modifiers,
            destination = sourceStructureRef,
            canReturn = false
          )
      }
    )

    result.ticksProcessed should be > 0
    result.walkerMoved should be > 0
    result.commoditiesDistributed should be(2)
    result.testCommodityRetrieved should be(true)
    result.testCommodityStored should be(true)
    result.testCommodityReturned should be(false)
  }

  it should "deliver commodities and return remaining home" in { _ =>
    val testProbe = TestProbe()
    testProbe.ignoreMsg {
      case _: AddProducer => true
      case _: AddConsumer => true
    }

    val result = setupGameMapAndCollectEvents(
      testProbe,
      parentStructure = new TestSourceStructure,
      targetStructure = new TestTargetStructure,
      walkerFactory = {
        case (parentStructureRef, sourceStructureRef) =>
          new TestDeliveryCarrier(
            properties.copy(parent = Some(parentStructureRef)),
            state.copy(
              path = Queue.empty
            ),
            modifiers,
            destination = sourceStructureRef,
            canReturn = true
          )
      }
    )

    result.ticksProcessed should be > 0
    result.walkerMoved should be > 0
    result.commoditiesDistributed should be(3)
    result.testCommodityRetrieved should be(true)
    result.testCommodityStored should be(true)
    result.testCommodityReturned should be(true)
  }

  private def setupGameMapAndCollectEvents(
    testProbe: TestProbe,
    parentStructure: Structure,
    targetStructure: Structure,
    walkerFactory: (StructureRef, StructureRef) => Walker
  ): Expectations = {
    val map = system.actorOf(
      Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 200.millis))
    )

    val parentStructurePoint = Point(0, 0)

    map.tell(CreateEntity(parentStructure, parentStructurePoint), testProbe.ref)
    testProbe.expectEntityCreatedAt(parentStructurePoint)
    val parentStructureRef = testProbe.receiveOne(timeout.duration).asInstanceOf[StructureRef]

    val targetStructurePoint = Point(2, 2)

    map.tell(CreateEntity(targetStructure, targetStructurePoint), testProbe.ref)
    testProbe.expectEntityCreatedAt(targetStructurePoint)
    val targetStructureRef = testProbe.receiveOne(timeout.duration).asInstanceOf[StructureRef]

    val walkerPoint = Point(0, 1)
    val walker = walkerFactory(parentStructureRef, targetStructureRef)

    map.tell(CreateEntity(walker, walkerPoint), testProbe.ref)
    testProbe.expectEntityCreatedAt(walkerPoint)
    testProbe.expectMsgType[WalkerRef]

    Expectations.fromList(
      testProbe
        .receiveWhile(timeout.duration) {
          case SystemEvent(Event.Engine.TickProcessed)                                    => TickProcessed
          case EntityEvent(Event.Engine.EntityMoved, _, _)                                => WalkerMoved
          case CellEvent(Event.Engine.MessageForwarded, _)                                => CommodityDistributed
          case CommodityInTransit(Commodity("TestCommodity"), Commodity.Amount(50), _, _) => CommodityRetrieved
          case CommodityInTransit(Commodity("TestCommodity"), Commodity.Amount(20), _, _) => CommodityStored
          case CommodityInTransit(Commodity("TestCommodity"), Commodity.Amount(0), _, _)  => CommodityReturned
          case _                                                                          => Ignored
        }
    )
  }
}

object CarrierSpec {
  private class TestSourceStructure() extends Structure {
    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: StructureRef =>
        StructureData(
          properties = Fixtures.Structure.Producing.properties,
          state = Fixtures.Structure.Producing.state.copy(
            risk = Structure.NoRisk,
            commodities = Structure.CommoditiesState(
              available = Map(Commodity("TestCommodity") -> Commodity.Amount(50)),
              limits = Map(Commodity("TestCommodity") -> Commodity.Amount(50))
            ),
            production = Structure.NoProduction
          ),
          modifiers = Fixtures.Structure.Producing.modifiers.copy(
            risk = Structure.NoRisk
          ),
          id
        )
    }

    override protected def createEffects(): Seq[(Data => Boolean, owe.effects.Effect)] = Seq.empty
    override protected def createBehaviour(): BaseStructure = new Industry {}
    override def `size`: Entity.Size = Entity.Size(1, 1)
    override def `desirability`: Entity.Desirability = Entity.Desirability.Neutral
  }

  private class TestTargetStructure() extends Structure {
    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: StructureRef =>
        StructureData(
          properties = Fixtures.Structure.Producing.properties,
          state = Fixtures.Structure.Producing.state.copy(
            risk = Structure.NoRisk,
            commodities = Structure.CommoditiesState(
              available = Map.empty,
              limits = Map(Commodity("TestCommodity") -> Commodity.Amount(30))
            ),
            production = Structure.NoProduction
          ),
          modifiers = Fixtures.Structure.Producing.modifiers.copy(
            risk = Structure.NoRisk
          ),
          id
        )
    }

    override protected def createEffects(): Seq[(Data => Boolean, owe.effects.Effect)] = Seq.empty
    override protected def createBehaviour(): BaseStructure = new Industry {}
    override def `size`: Entity.Size = Entity.Size(1, 1)
    override def `desirability`: Entity.Desirability = Entity.Desirability.Neutral
  }

  private class TestRetrievalCarrier(
    properties: Properties,
    state: State,
    modifiers: StateModifiers,
    source: StructureRef,
    canReturn: Boolean
  ) extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint

    override protected def createBehaviour(): BaseWalker = new Carrier {
      override protected def actions: Seq[BaseWalker.Action] =
        properties.parent match {
          case Some(parent) => retrieve(source, parent)
          case None         => throw new IllegalStateException("Test Carrier requires parent structure")
        }

      override protected def canReturnCommodities: Boolean = canReturn
    }

    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: WalkerRef =>
        WalkerData(properties, state, modifiers, id)
    }

    override protected def createEffects(): Seq[(Data => Boolean, owe.effects.Effect)] = Seq.empty
  }

  private class TestDeliveryCarrier(
    properties: Properties,
    state: State,
    modifiers: StateModifiers,
    destination: StructureRef,
    canReturn: Boolean
  ) extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint

    override protected def createBehaviour(): BaseWalker = new Carrier {
      override protected def actions: Seq[BaseWalker.Action] =
        properties.parent match {
          case Some(parent) => deliver(parent, destination)
          case None         => throw new IllegalStateException("Test Carrier requires parent structure")
        }

      override protected def canReturnCommodities: Boolean = canReturn
    }

    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: WalkerRef =>
        WalkerData(properties, state, modifiers, id)
    }

    override protected def createEffects(): Seq[(Data => Boolean, owe.effects.Effect)] = Seq.empty
  }

  case class Expectations(
    ticksProcessed: Int,
    walkerMoved: Int,
    commoditiesDistributed: Int,
    testCommodityRetrieved: Boolean,
    testCommodityStored: Boolean,
    testCommodityReturned: Boolean
  )

  object Expectations {
    import Expectation._

    def empty: Expectations = Expectations(
      ticksProcessed = 0,
      walkerMoved = 0,
      commoditiesDistributed = 0,
      testCommodityRetrieved = false,
      testCommodityStored = false,
      testCommodityReturned = false
    )

    def fromList(expectations: Seq[Expectation]): Expectations =
      expectations.foldLeft(Expectations.empty) {
        case (exps, current) =>
          current match {
            case TickProcessed        => exps.copy(ticksProcessed = exps.ticksProcessed + 1)
            case WalkerMoved          => exps.copy(walkerMoved = exps.walkerMoved + 1)
            case CommodityDistributed => exps.copy(commoditiesDistributed = exps.commoditiesDistributed + 1)
            case CommodityRetrieved   => exps.copy(testCommodityRetrieved = true)
            case CommodityStored      => exps.copy(testCommodityStored = true)
            case CommodityReturned    => exps.copy(testCommodityReturned = true)
            case Ignored              => exps
          }
      }
  }

  sealed trait Expectation
  object Expectation {
    case object TickProcessed extends Expectation
    case object WalkerMoved extends Expectation
    case object CommodityDistributed extends Expectation
    case object CommodityRetrieved extends Expectation
    case object CommodityStored extends Expectation
    case object CommodityReturned extends Expectation
    case object Ignored extends Expectation
  }
}
