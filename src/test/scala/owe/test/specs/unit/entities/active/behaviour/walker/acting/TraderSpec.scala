package owe.test.specs.unit.entities.active.behaviour.walker.acting

import akka.actor.Props
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, StructureData, WalkerData}
import owe.entities.Entity
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker._
import owe.entities.active.attributes.{Life, Speed}
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.structure.producing.Industry
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.acting.Trader
import owe.entities.active.{Structure, Walker}
import owe.events.Event
import owe.map.GameMap.CreateEntity
import owe.map.grid.Point
import owe.production.Commodity
import owe.production.Commodity.State.Lost
import owe.production.Exchange.UpdateCommodityState
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures
import owe.test.specs.unit.map.TestGameMap
import owe.test.specs.unit.map.TestGameMap.StartBehaviour
import scala.collection.immutable.Queue
import scala.concurrent.duration._

import owe.events.Event.{CellEvent, EntityEvent, SystemEvent}
import owe.test.specs.unit.entities.EntityTestHelpers

class TraderSpec extends AkkaUnitSpec("TraderSpec") with EntityTestHelpers {
  import TraderSpec.Expectation._
  import TraderSpec._

  protected implicit val timeout: Timeout = 5.seconds

  private val properties: Properties = Fixtures.Walker.properties.copy(
    homePosition = Point(0, 2),
    name = "Trader",
    movementSpeed = Speed(1)
  )

  private val state: State = Fixtures.Walker.state.copy(
    currentLife = Life(50),
    commodities = CommoditiesState(
      available = Map(
        Commodity("TestCommodity#1") -> Commodity.Amount(100),
        Commodity("TestCommodity#3") -> Commodity.Amount(100)
      ),
      limits = Map(
        Commodity("TestCommodity#1") -> Commodity.Amount(100),
        Commodity("TestCommodity#2") -> Commodity.Amount(100),
        Commodity("TestCommodity#3") -> Commodity.Amount(100)
      )
    ),
    mode = MovementMode.Advancing
  )

  private val modifiers: StateModifiers = Fixtures.Walker.modifiers

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Trader walker" should "go to destination and load/unload commodities" in { _ =>
    val testProbe = TestProbe()
    val map = system.actorOf(
      Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 300.millis))
    )

    val structurePoint = Point(0, 0)
    val structure = new TestStructure

    map.tell(CreateEntity(structure, structurePoint), testProbe.ref)
    testProbe.expectEntityCreatedAt(structurePoint)
    val structureRef = testProbe.receiveOne(timeout.duration).asInstanceOf[StructureRef]

    val walkerPoint = Point(0, 1)
    val walker = new TestTrader(
      properties,
      state.copy(
        path = Queue.empty
      ),
      modifiers,
      destination = structureRef,
      commoditiesToSell = Seq(Commodity("TestCommodity#1")),
      commoditiesToBuy = Seq(Commodity("TestCommodity#2"))
    )

    map.tell(CreateEntity(walker, walkerPoint), testProbe.ref)
    testProbe.expectEntityCreatedAt(walkerPoint)
    testProbe.expectMsgType[WalkerRef]

    val result = testProbe
      .receiveWhile(timeout.duration) {
        case SystemEvent(Event.Engine.TickProcessed)                                         => TickProcessed
        case EntityEvent(Event.Engine.EntityMoved, _, _)                                     => WalkerMoved
        case CellEvent(Event.Engine.MessageForwarded, _)                                     => CommodityDistributed
        case UpdateCommodityState(Commodity("TestCommodity#1"), Commodity.Amount(70), Lost)  => CommoditiesReturned
        case UpdateCommodityState(Commodity("TestCommodity#2"), Commodity.Amount(40), Lost)  => CommoditiesBought
        case UpdateCommodityState(Commodity("TestCommodity#3"), Commodity.Amount(100), Lost) => CommoditiesNotTraded
        case EntityEvent(Event.Engine.EntityDestroyed, _, _)                                 => TraderLeft
        case _                                                                               => Ignored
      }
      .foldLeft(Expectations.empty) {
        case (exps, current) =>
          current match {
            case TickProcessed        => exps.copy(ticksProcessed = exps.ticksProcessed + 1)
            case WalkerMoved          => exps.copy(walkerMoved = exps.walkerMoved + 1)
            case CommodityDistributed => exps.copy(commoditiesDistributed = exps.commoditiesDistributed + 1)
            case CommoditiesReturned  => exps.copy(commoditiesReturned = true)
            case CommoditiesNotTraded => exps.copy(commoditiesNotTraded = true)
            case CommoditiesBought    => exps.copy(commoditiesBought = true)
            case TraderLeft           => exps.copy(traderLeft = true)
            case Ignored              => exps
          }
      }

    result.ticksProcessed should be > 0
    result.walkerMoved should be > 0
    result.commoditiesDistributed should be(2)
    result.commoditiesReturned should be(true)
    result.commoditiesNotTraded should be(true)
    result.commoditiesBought should be(true)
    result.traderLeft should be(true)
  }
}

object TraderSpec {
  private class TestStructure() extends Structure {
    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: StructureRef =>
        StructureData(
          properties = Fixtures.Structure.Producing.properties,
          state = Fixtures.Structure.Producing.state.copy(
            commodities = Structure.CommoditiesState(
              available = Map(Commodity("TestCommodity#2") -> Commodity.Amount(40)),
              limits = Map(Commodity("TestCommodity#1") -> Commodity.Amount(30))
            ),
            production = Structure.NoProduction
          ),
          modifiers = Fixtures.Structure.Producing.modifiers,
          id
        )
    }

    override protected def createEffects(): Seq[(Data => Boolean, owe.effects.Effect)] = Seq.empty
    override protected def createBehaviour(): BaseStructure = new Industry {}
    override def `size`: Entity.Size = Entity.Size(1, 1)
    override def `desirability`: Entity.Desirability = Entity.Desirability.Neutral
  }

  private class TestTrader(
    properties: Properties,
    state: State,
    modifiers: StateModifiers,
    destination: StructureRef,
    commoditiesToSell: Seq[Commodity],
    commoditiesToBuy: Seq[Commodity]
  ) extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint

    override protected def createBehaviour(): BaseWalker = new Trader {
      override protected def target: Structure.StructureRef = destination
      override protected def selling: Seq[Commodity] = commoditiesToSell
      override protected def buying: Seq[Commodity] = commoditiesToBuy
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
    commoditiesReturned: Boolean,
    commoditiesNotTraded: Boolean,
    commoditiesBought: Boolean,
    traderLeft: Boolean
  )

  object Expectations {
    def empty: Expectations = Expectations(
      ticksProcessed = 0,
      walkerMoved = 0,
      commoditiesDistributed = 0,
      commoditiesReturned = false,
      commoditiesNotTraded = false,
      commoditiesBought = false,
      traderLeft = false
    )
  }

  sealed trait Expectation
  object Expectation {
    case object TickProcessed extends Expectation
    case object WalkerMoved extends Expectation
    case object CommodityDistributed extends Expectation
    case object CommoditiesReturned extends Expectation
    case object CommoditiesNotTraded extends Expectation
    case object CommoditiesBought extends Expectation
    case object TraderLeft extends Expectation
    case object Ignored extends Expectation
  }
}
