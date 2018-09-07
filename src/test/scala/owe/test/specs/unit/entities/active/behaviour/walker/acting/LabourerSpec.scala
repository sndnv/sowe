package owe.test.specs.unit.entities.active.behaviour.walker.acting

import akka.actor.Props
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntity._
import owe.entities.Entity
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker._
import owe.entities.active.attributes.{Life, Speed}
import owe.entities.active.behaviour.resource.BaseResource
import owe.entities.active.behaviour.resource.producing.ProducingResource
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.structure.producing.Industry
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker.GoToPoint
import owe.entities.active.behaviour.walker.acting.Labourer
import owe.entities.active.{Resource, Structure, Walker}
import owe.events.Event
import owe.map.GameMap.CreateEntity
import owe.map.grid.Point
import owe.production.Commodity
import owe.production.Exchange.{CommodityInTransit, UpdateCommodityState}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures
import owe.test.specs.unit.map.TestGameMap
import owe.test.specs.unit.map.TestGameMap.StartBehaviour
import scala.collection.immutable.Queue
import scala.concurrent.duration._

import owe.events.Event.{CellEvent, EntityEvent, SystemEvent}
import owe.test.specs.unit.entities.EntityTestHelpers

class LabourerSpec extends AkkaUnitSpec("LabourerSpec") with EntityTestHelpers {
  import LabourerSpec._
  import LabourerSpec.Expectation._

  protected implicit val timeout: Timeout = 5.seconds

  private val properties: Properties = Fixtures.Walker.properties.copy(
    homePosition = Point(0, 0),
    name = "Labourer",
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

  "A Labourer walker" should "go to resources, gather commodities and return them home" in { _ =>
    val testProbe = TestProbe()
    val map = system.actorOf(
      Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 300.millis))
    )

    val structurePoint = Point(0, 0)
    val structure = new TestStructure

    map.tell(CreateEntity(structure, structurePoint), testProbe.ref)
    testProbe.expectEntityCreatedAt(structurePoint)
    val structureRef = testProbe.receiveOne(timeout.duration).asInstanceOf[StructureRef]

    val resourcePoint = Point(2, 2)
    val resource = new TestResource

    map.tell(CreateEntity(resource, resourcePoint), testProbe.ref)
    testProbe.expectEntityCreatedAt(resourcePoint)
    val resourceRef = testProbe.receiveOne(timeout.duration).asInstanceOf[ResourceRef]

    val walkerPoint = Point(0, 1)
    val walker = new TestLabourer(
      properties.copy(parent = Some(structureRef)),
      state.copy(
        path = Queue.empty
      ),
      modifiers,
      destination = Right(resourceRef)
    )

    map.tell(CreateEntity(walker, walkerPoint), testProbe.ref)
    testProbe.expectEntityCreatedAt(walkerPoint)
    testProbe.expectMsgType[WalkerRef]

    val result = testProbe
      .receiveWhile(timeout.duration) {
        case SystemEvent(Event.Engine.TickProcessed)                                       => TickProcessed
        case EntityEvent(Event.Engine.EntityMoved, _, _)                                   => WalkerMoved
        case CellEvent(Event.Engine.MessageForwarded, _)                                   => CommodityDistributed
        case UpdateCommodityState(Commodity("TestCommodity"), _, Commodity.State.Produced) => CommodityProduced
        case CommodityInTransit(Commodity("TestCommodity"), Commodity.Amount(100), _, _)   => CommodityRetrieved
        case CommodityInTransit(Commodity("TestCommodity"), Commodity.Amount(70), _, _)    => CommodityStored
        case _                                                                             => Ignored
      }
      .foldLeft(Expectations.empty) {
        case (exps, current) =>
          current match {
            case TickProcessed        => exps.copy(ticksProcessed = exps.ticksProcessed + 1)
            case WalkerMoved          => exps.copy(walkerMoved = exps.walkerMoved + 1)
            case CommodityDistributed => exps.copy(commoditiesDistributed = exps.commoditiesDistributed + 1)
            case CommodityProduced    => exps.copy(testCommodityProduced = exps.testCommodityProduced + 1)
            case CommodityRetrieved   => exps.copy(testCommodityRetrieved = true)
            case CommodityStored      => exps.copy(testCommodityStored = true)
            case Ignored              => exps
          }
      }

    result.ticksProcessed should be > 0
    result.walkerMoved should be > 0
    result.commoditiesDistributed should be(2)
    result.testCommodityProduced should be > 0
    result.testCommodityRetrieved should be(true)
    result.testCommodityStored should be(true)
  }

  it should "go to a destination and perform action" in { _ =>
    val testProbe = TestProbe()
    val map = system.actorOf(
      Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 500.millis))
    )

    val destination = Point(0, 2)
    val walker = new TestLabourer(
      properties,
      state,
      modifiers,
      destination = Left(destination)
    )

    val walkerPoint = Point(2, 0)

    map.tell(CreateEntity(walker, walkerPoint), testProbe.ref)
    testProbe.expectEntityCreatedAt(walkerPoint)
    testProbe.expectMsgType[WalkerRef]

    val (ticks, moved) = testProbe
      .receiveWhile(timeout.duration) {
        case SystemEvent(Event.Engine.TickProcessed)                 => (1, 0)
        case EntityEvent(Event.Engine.EntityMoved, _, `destination`) => (0, 1)
        case _                                                       => (0, 0)
      }
      .foldLeft((0, 0)) {
        case ((totalTicks, totalMoved), (currentTick, currentMoved)) =>
          (totalTicks + currentTick, totalMoved + currentMoved)
      }

    ticks should be > 0
    moved should be(1)
  }
}

object LabourerSpec {
  private class TestStructure() extends Structure {
    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: StructureRef =>
        StructureData(
          properties = Fixtures.Structure.Producing.properties,
          state = Fixtures.Structure.Producing.state.copy(
            commodities = Structure.CommoditiesState(
              available = Map.empty,
              limits = Map(Commodity("TestCommodity") -> Commodity.Amount(30))
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

  private class TestResource() extends Resource {
    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: ResourceRef =>
        ResourceData(
          Fixtures.Resource.properties,
          Fixtures.Resource.state,
          Fixtures.Resource.modifiers,
          id
        )
    }

    override protected def createEffects(): Seq[(Data => Boolean, owe.effects.Effect)] = Seq.empty

    override protected def createBehaviour(): BaseResource = new ProducingResource {}
  }

  private class TestLabourer(
    properties: Properties,
    state: State,
    modifiers: StateModifiers,
    destination: Either[Point, ResourceRef]
  ) extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint

    override protected def createBehaviour(): BaseWalker = new Labourer {
      override protected def actions: Seq[BaseWalker.Action] = destination match {
        case Left(point) =>
          Seq(GoToPoint(point))

        case Right(resource) =>
          properties.parent match {
            case Some(parent) => gatherResources(resource, parent)
            case None         => throw new IllegalStateException("Test Labourer requires parent structure")
          }
      }
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
    testCommodityProduced: Int,
    testCommodityRetrieved: Boolean,
    testCommodityStored: Boolean
  )

  object Expectations {
    def empty: Expectations = Expectations(
      ticksProcessed = 0,
      walkerMoved = 0,
      commoditiesDistributed = 0,
      testCommodityProduced = 0,
      testCommodityRetrieved = false,
      testCommodityStored = false
    )
  }

  sealed trait Expectation
  object Expectation {
    case object TickProcessed extends Expectation
    case object WalkerMoved extends Expectation
    case object CommodityDistributed extends Expectation
    case object CommodityProduced extends Expectation
    case object CommodityRetrieved extends Expectation
    case object CommodityStored extends Expectation
    case object Ignored extends Expectation
  }
}
