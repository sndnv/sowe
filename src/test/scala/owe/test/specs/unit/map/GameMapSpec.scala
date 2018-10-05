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
import owe.events.Event.{CellEvent, EntityEvent, SystemEvent}
import owe.map.Cell.CellData
import owe.map.GameMap._
import owe.map.grid.{Grid, Point}
import owe.production.Commodity
import owe.production.Exchange.{AddProducer, GetExchangeStats}
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.EntityTestHelpers
import owe.test.specs.unit.entities.definitions.active.resources.Tree
import owe.test.specs.unit.entities.definitions.active.structures.CoalMine
import owe.test.specs.unit.map.TestGameMap.StartBehaviour

import scala.collection.immutable.Queue
import scala.concurrent.duration._

class GameMapSpec extends AkkaUnitSpec("GameMapSpec") with EntityTestHelpers {

  protected implicit val timeout: Timeout = 3.seconds

  case class FixtureParam(testProbe: TestProbe)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam(TestProbe())))

  "A GameMap" should "process, complete and expire game ticks when active" in { fixture =>
    val map = system.actorOf(
      Props(
        new TestGameMap(
          fixture.testProbe.ref,
          StartBehaviour.Idle,
          interval = 1.second,
          expiration = 1.second
        )
      )
    )

    fixture.testProbe.expectMsg(SystemEvent(Event.Engine.TickProcessed))

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

    map.tell(CreateEntity(new NonResponsiveEntity, (0, 0)), fixture.testProbe.ref)

    fixture.testProbe.receiveWhile(timeout.duration) {
      case EntityEvent(Event.Engine.EntityCreated, _, Point(0, 0)) => ()
      case _: ResourceRef                                          => ()
      case SystemEvent(Event.Engine.TickExpired)                   => ()
      case _: AddProducer                                          => ()
      case message                                                 => fail(s"Unexpected message encountere: [$message]")
    }
  }

  it should "respond with advance paths when waiting" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map.tell(GetAdvancePath(entityRef, (2, 2)), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(Queue.empty)
  }

  it should "respond with roaming paths when waiting" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map.tell(GetRoamingPath(entityRef, Distance(10)), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(Queue.empty)
  }

  it should "respond with neighbours when waiting" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map.tell(GetNeighbours(entityRef, Distance(10)), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(Seq.empty)
  }

  it should "respond with multiple entities when waiting" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Waiting)))

    map.tell(GetEntities((0, 1)), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(Seq.empty)
  }

  it should "respond with single entities when waiting" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map.tell(GetEntity(entityRef), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[Status.Failure]
  }

  it should "respond with adjacent entity points when waiting" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Waiting)))

    val entityRef = WalkerRef(TestProbe().ref)

    map.tell(GetAdjacentPoint(entityRef, _.isFree), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(None)
  }

  it should "log unexpected entity tick responses when waiting" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Waiting)))
    map.tell(EntityTickProcessed(tick = 42), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(SystemEvent(Event.Engine.UnexpectedEntityResponseReceived))
  }

  it should "create entities when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    val entity = new Road()

    map.tell(CreateEntity(entity, (0, 0)), fixture.testProbe.ref)
    fixture.testProbe.expectEntityCreatedAt(0, 0)
    fixture.testProbe.expectMsgType[RoadRef]

    map.tell(CreateEntity(entity, (13, 5)), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(CellEvent(Event.Engine.CellOutOfBounds, (13, 5)))
  }

  it should "destroy entities when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    val entity = new Road()

    map.tell(CreateEntity(entity, (0, 0)), fixture.testProbe.ref)
    fixture.testProbe.expectEntityCreatedAt(0, 0)

    val entityRef = fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[RoadRef]

    map.tell(DestroyEntity(entityRef), fixture.testProbe.ref)
    fixture.testProbe.expectEntityDestroyedAt((0, 0))

    map.tell(DestroyEntity(WalkerRef(TestProbe().ref)), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(SystemEvent(Event.Engine.CellOutOfBounds))
  }

  it should "move entities when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    val entity = new Road()

    map.tell(CreateEntity(entity, (0, 0)), fixture.testProbe.ref)
    fixture.testProbe.expectEntityCreatedAt(0, 0)

    val entityRef = fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[RoadRef]

    map.tell(MoveEntity(entityRef, (0, 1)), fixture.testProbe.ref)
    fixture.testProbe.expectEntityMovedTo((0, 1))

    map.tell(MoveEntity(entityRef, (13, 5)), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(CellEvent(Event.Engine.CellOutOfBounds, (13, 5)))
  }

  it should "process commodity distribution when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    val entity = new Tree()

    map.tell(CreateEntity(entity, (0, 0)), fixture.testProbe.ref)

    val entityRef = fixture.testProbe
      .receiveWhile(timeout.duration) {
        case ref: ResourceRef                                        => Some(ref)
        case EntityEvent(Event.Engine.EntityCreated, _, Point(0, 0)) => None
        case _: AddProducer                                          => None
        case message                                                 => fail(s"Unexpected message encountered: [$message]")
      }
      .flatten
      .head

    map.tell(DistributeCommodities(entityRef, Seq.empty), fixture.testProbe.ref)
    fixture.testProbe.expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "process entity attacks when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    val entity = new Tree()

    map.tell(CreateEntity(entity, (0, 0)), fixture.testProbe.ref)

    val entityRef = fixture.testProbe
      .receiveWhile(timeout.duration) {
        case ref: ResourceRef                                        => Some(ref)
        case EntityEvent(Event.Engine.EntityCreated, _, Point(0, 0)) => None
        case _: AddProducer                                          => None
        case message                                                 => fail(s"Unexpected message encountered: [$message]")
      }
      .flatten
      .head

    map.tell(AttackEntity(entityRef, AttackDamage(0)), fixture.testProbe.ref)
    fixture.testProbe.expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "process labour found updates when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    val entity = new CoalMine()

    map.tell(CreateEntity(entity, (0, 0)), fixture.testProbe.ref)

    val entityRef = fixture.testProbe
      .receiveWhile(timeout.duration) {
        case ref: StructureRef                                       => Some(ref)
        case EntityEvent(Event.Engine.EntityCreated, _, Point(0, 0)) => None
        case _: AddProducer                                          => None
        case message                                                 => fail(s"Unexpected message encountered: [$message]")
      }
      .flatten
      .head

    map.tell(LabourFound(entityRef), fixture.testProbe.ref)
    fixture.testProbe.expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "process occupants updates when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    val entity = new CoalMine()

    map.tell(CreateEntity(entity, (0, 0)), fixture.testProbe.ref)

    val entityRef = fixture.testProbe
      .receiveWhile(timeout.duration) {
        case ref: StructureRef                                       => Some(ref)
        case EntityEvent(Event.Engine.EntityCreated, _, Point(0, 0)) => None
        case _: AddProducer                                          => None
        case message                                                 => fail(s"Unexpected message encountered: [$message]")
      }
      .flatten
      .head

    map.tell(OccupantsUpdate(entityRef, occupants = 10), fixture.testProbe.ref)
    fixture.testProbe.expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "process labour updates when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    val entity = new CoalMine()

    map.tell(CreateEntity(entity, (0, 0)), fixture.testProbe.ref)

    val entityRef = fixture.testProbe
      .receiveWhile(timeout.duration) {
        case ref: StructureRef                                       => Some(ref)
        case EntityEvent(Event.Engine.EntityCreated, _, Point(0, 0)) => None
        case _: AddProducer                                          => None
        case message                                                 => fail(s"Unexpected message encountered: [$message]")
      }
      .flatten
      .head

    map.tell(LabourUpdate(entityRef, employees = 10), fixture.testProbe.ref)
    fixture.testProbe.expectMsgAllOf(CellEvent(Event.Engine.MessageForwarded, (0, 0)))
  }

  it should "forward messages to commodity exchange when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    map.tell(ForwardExchangeMessage(GetExchangeStats()), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(GetExchangeStats())
  }

  it should "log entity tick responses when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    map.tell(EntityTickProcessed(tick = 0), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(SystemEvent(Event.Engine.UnexpectedEntityResponseReceived))
  }

  it should "respond with grid data when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))
    map.tell(GetGrid(), fixture.testProbe.ref)

    val grid = fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[Grid[CellData]]
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

  it should "respond with multiple entities when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))

    map.tell(GetEntities((0, 1)), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(Seq.empty)
  }

  it should "respond with single entities when idle" in { fixture =>
    val map = system.actorOf(Props(new TestGameMap(fixture.testProbe.ref, StartBehaviour.Idle)))

    val entityRef = WalkerRef(TestProbe().ref)

    map.tell(GetEntity(entityRef), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[Status.Failure]
  }
}
