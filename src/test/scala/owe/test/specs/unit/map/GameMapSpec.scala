package owe.test.specs.unit.map

import akka.Done
import akka.actor.{Props, Status}
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.attributes.{AttackDamage, Distance}
import owe.entities.passive.Road
import owe.entities.passive.Road.RoadRef
import owe.events.Event
import owe.map.GameMap._
import owe.production.Exchange.GetExchangeStats
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.definitions.active.resources.Tree
import owe.test.specs.unit.entities.definitions.active.structures.CoalMine
import owe.test.specs.unit.map.TestGameMap.StartBehaviour

import scala.collection.immutable.Queue
import scala.concurrent.duration._

class GameMapSpec extends AkkaUnitSpec("GameMapSpec") {

  private implicit val timeout: Timeout = 3.seconds

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A GameMap" should "process and complete a game ticks when active" in { _ =>
    val _ = system.actorOf(
      Props(
        new TestGameMap(testActor, StartBehaviour.Idle, interval = 1.second)
      )
    )

    expectMsg(Event(Event.System.TickProcessed, cell = None))
  }

  it should "respond with advance paths when active" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Active)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetAdvancePath(entityRef, (2, 2))
    expectMsg(Queue.empty)
  }

  it should "respond with roaming paths when active" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Active)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetRoamingPath(entityRef, Distance(10))
    expectMsg(Queue.empty)
  }

  it should "respond with neighbours when active" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Active)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetNeighbours(entityRef, Distance(10))
    expectMsg(Seq.empty)
  }

  it should "respond with multiple entities when active" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Active)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetEntities((0, 1))
    expectMsg(Seq.empty)
  }

  it should "respond with single entities when active" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Active)))

    val entityRef = WalkerRef(TestProbe().ref)

    map ! GetEntity(entityRef)
    expectMsgType[Status.Failure]
  }

  it should "create entities when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Road()

    map ! CreateEntity(entity, (0, 0))
    expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))
    expectMsgType[RoadRef]
  }

  it should "destroy entities when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Road()

    map ! CreateEntity(entity, (0, 0))
    expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))

    val entityRef = receiveOne(timeout.duration).asInstanceOf[RoadRef]

    map ! DestroyEntity(entityRef)
    expectMsg(Event(Event.System.EntityDestroyed, Some((0, 0))))
  }

  it should "move entities when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Road()

    map ! CreateEntity(entity, (0, 0))
    expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))

    val entityRef = receiveOne(timeout.duration).asInstanceOf[RoadRef]

    map ! MoveEntity(entityRef, (0, 1))
    expectMsg(Event(Event.System.EntityMoved, Some((0, 1))))
  }

  it should "process commodity distribution when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Tree()

    map ! CreateEntity(entity, (0, 0))
    expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))

    val entityRef = receiveOne(timeout.duration).asInstanceOf[ResourceRef]

    map ! DistributeCommodities(entityRef, Seq.empty)
    expectMsgAllOf(Done, Event(Event.System.MessageForwarded, Some((0, 0))))
  }

  it should "process entity attacks when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new Tree()

    map ! CreateEntity(entity, (0, 0))
    expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))

    val entityRef = receiveOne(timeout.duration).asInstanceOf[ResourceRef]

    map ! AttackEntity(entityRef, AttackDamage(0))
    expectMsgAllOf(Done, Event(Event.System.MessageForwarded, Some((0, 0))))
  }

  it should "process labour found updates when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new CoalMine()

    map ! CreateEntity(entity, (0, 0))
    expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))

    val entityRef = receiveOne(timeout.duration).asInstanceOf[StructureRef]

    map ! LabourFound(entityRef)
    expectMsgAllOf(Done, Event(Event.System.MessageForwarded, Some((0, 0))))
  }

  it should "process occupants updates when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new CoalMine()

    map ! CreateEntity(entity, (0, 0))
    expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))

    val entityRef = receiveOne(timeout.duration).asInstanceOf[StructureRef]

    map ! OccupantsUpdate(entityRef, occupants = 10)
    expectMsgAllOf(Done, Event(Event.System.MessageForwarded, Some((0, 0))))
  }

  it should "process labour updates when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    val entity = new CoalMine()

    map ! CreateEntity(entity, (0, 0))
    expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))

    val entityRef = receiveOne(timeout.duration).asInstanceOf[StructureRef]

    map ! LabourUpdate(entityRef, employees = 10)
    expectMsgAllOf(Done, Event(Event.System.MessageForwarded, Some((0, 0))))
  }

  it should "forward messages to commodity exchange when idle" in { _ =>
    val map = system.actorOf(Props(new TestGameMap(testActor, StartBehaviour.Idle)))
    map ! ForwardExchangeMessage(GetExchangeStats())
    expectMsg(GetExchangeStats())
  }
}
