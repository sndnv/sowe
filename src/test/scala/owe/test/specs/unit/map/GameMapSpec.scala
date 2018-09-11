package owe.test.specs.unit.map

import akka.actor.{Props, Status}
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, ResourceData}
import owe.entities.active.Resource
import owe.entities.active.Resource.{Properties, ResourceRef, State, StateModifiers}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.attributes.{AttackDamage, Distance}
import owe.entities.active.behaviour.resource.producing.ProducingResource
import owe.entities.passive.Road
import owe.entities.passive.Road.RoadRef
import owe.events.Event
import owe.map.GameMap._
import owe.map.grid.{Grid, Point}
import owe.production.Commodity
import owe.production.Exchange.GetExchangeStats
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.definitions.active.resources.Tree
import owe.test.specs.unit.entities.definitions.active.structures.CoalMine
import owe.test.specs.unit.map.TestGameMap.StartBehaviour
import scala.collection.immutable.Queue
import scala.concurrent.duration._

import owe.events.Event.{CellEvent, EntityEvent, SystemEvent}
import owe.map.Cell.{Availability, CellData}
import owe.test.specs.unit.entities.EntityTestHelpers

class GameMapSpec extends AkkaUnitSpec("GameMapSpec") with EntityTestHelpers {

  protected implicit val timeout: Timeout = 3.seconds

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A GameMap" should "process, complete and expire game ticks when active" in { _ =>
    val map = system.actorOf(
      Props(
        new TestGameMap(
          testActor,
          StartBehaviour.Idle,
          interval = 1.second,
          expiration = 1.second
        )
      )
    )

    expectMsg(SystemEvent(Event.Engine.TickProcessed))

    class NonResponsiveEntity extends Resource {
      override protected def createActiveEntityData(): ActiveEntityRef => Data = {
        case id: ResourceRef =>
          ResourceData(
            properties = Properties(
              name = "Tree",
              homePosition = Point(0, 0),
              commodity = Commodity("Wood"),
              maxAmount = Commodity.Amount(500)
            ),
            state = State(
              currentAmount = Commodity.Amount(0),
              replenishAmount = Commodity.Amount(25)
            ),
            modifiers = StateModifiers(
              replenishAmount = Commodity.AmountModifier(100)
            ),
            id
          )
      }

      override protected def createBehaviour(): ProducingResource = new ProducingResource {
        override protected def behaviour: Behaviour = {
          case _ => () // ignored
        }
      }

      override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
    }

    map ! CreateEntity(new NonResponsiveEntity, (0, 0))
    this.expectEntityCreatedAt((0, 0))
    expectMsgType[ResourceRef]

    expectMsg(SystemEvent(Event.Engine.TickExpired))
  }

  it should "respond with advance paths when waiting" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetAdvancePath(entityRef, (2, 2))
    expectMsg(Queue.empty)
  }

  it should "respond with roaming paths when waiting" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetRoamingPath(entityRef, Distance(10))
    expectMsg(Queue.empty)
  }

  it should "respond with neighbours when waiting" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetNeighbours(entityRef, Distance(10))
    expectMsg(Seq.empty)
  }

  it should "respond with multiple entities when waiting" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Waiting)))

    map ! GetEntities((0, 1))
    expectMsg(Seq.empty)
  }

  it should "respond with single entities when waiting" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetEntity(entityRef)
    expectMsgType[Status.Failure]
  }

  it should "respond with adjacent entity roads when waiting" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetAdjacentRoad(entityRef)
    expectMsg(None)
  }

  it should "respond with adjacent entity points when waiting" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetAdjacentPoint(entityRef, Availability.Buildable)
    expectMsg(None)
  }

  it should "log unexpected entity tick responses when waiting" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Waiting)))
    map ! EntityTickProcessed(tick = 42)
    expectMsg(SystemEvent(Event.Engine.UnexpectedEntityResponseReceived))
  }

  it should "create entities when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Road()

    map ! CreateEntity(entity, (0, 0))
    this.expectEntityCreatedAt(0, 0)
    expectMsgType[RoadRef]

    map ! CreateEntity(entity, (13, 5))
    expectMsg(CellEvent(Event.Engine.CellOutOfBounds, (13, 5)))
  }

  it should "destroy entities when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Road()

    map ! CreateEntity(entity, (0, 0))
    this.expectEntityCreatedAt(0, 0)

    val entityRef = receiveOne(timeout.duration).asInstanceOf[RoadRef]

    map ! DestroyEntity(entityRef)
    this.expectEntityDestroyedAt((0, 0))

    map ! DestroyEntity(WalkerRef(TestProbe().ref))
    expectMsg(SystemEvent(Event.Engine.CellOutOfBounds))
  }

  it should "move entities when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Road()

    map ! CreateEntity(entity, (0, 0))
    this.expectEntityCreatedAt(0, 0)

    val entityRef = receiveOne(timeout.duration).asInstanceOf[RoadRef]

    map ! MoveEntity(entityRef, (0, 1))
    this.expectEntityMovedTo((0, 1))

    map ! MoveEntity(entityRef, (13, 5))
    expectMsg(CellEvent(Event.Engine.CellOutOfBounds, (13, 5)))
  }

  it should "process commodity distribution when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Tree()

    map ! CreateEntity(entity, (0, 0))
    this.expectEntityCreatedAt(0, 0)

    val entityRef = receiveOne(timeout.duration).asInstanceOf[ResourceRef]

    map ! DistributeCommodities(entityRef, Seq.empty)
    expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "process entity attacks when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Tree()

    map ! CreateEntity(entity, (0, 0))
    this.expectEntityCreatedAt(0, 0)

    val entityRef = receiveOne(timeout.duration).asInstanceOf[ResourceRef]

    map ! AttackEntity(entityRef, AttackDamage(0))
    expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "process labour found updates when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new CoalMine()

    map ! CreateEntity(entity, (0, 0))
    this.expectEntityCreatedAt(0, 0)

    val entityRef = receiveOne(timeout.duration).asInstanceOf[StructureRef]

    map ! LabourFound(entityRef)
    expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "process occupants updates when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new CoalMine()

    map ! CreateEntity(entity, (0, 0))
    this.expectEntityCreatedAt(0, 0)

    val entityRef = receiveOne(timeout.duration).asInstanceOf[StructureRef]

    map ! OccupantsUpdate(entityRef, occupants = 10)
    expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "process labour updates when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new CoalMine()

    map ! CreateEntity(entity, (0, 0))
    this.expectEntityCreatedAt(0, 0)

    val entityRef = receiveOne(timeout.duration).asInstanceOf[StructureRef]

    map ! LabourUpdate(entityRef, employees = 10)
    expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "forward messages to commodity exchange when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    map ! ForwardExchangeMessage(GetExchangeStats())
    expectMsg(GetExchangeStats())
  }

  it should "log entity tick responses when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    map ! EntityTickProcessed(tick = 0)
    expectMsg(SystemEvent(Event.Engine.UnexpectedEntityResponseReceived))
  }

  it should "respond with grid data when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    map ! GetGrid()

    val grid = receiveOne(timeout.duration).asInstanceOf[Grid[CellData]]
    grid.toMap should be(
      Map(
        Point(0, 0) -> CellData.empty,
        Point(1, 0) -> CellData.empty,
        Point(2, 0) -> CellData.empty,
        Point(0, 1) -> CellData.empty,
        Point(1, 1) -> CellData.empty,
        Point(2, 1) -> CellData.empty,
        Point(0, 2) -> CellData.empty,
        Point(1, 2) -> CellData.empty,
        Point(2, 2) -> CellData.empty
      )
    )
  }

  it should "respond with multiple entities when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))

    map ! GetEntities((0, 1))
    expectMsg(Seq.empty)
  }

  it should "respond with single entities when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetEntity(entityRef)
    expectMsgType[Status.Failure]
  }
}
