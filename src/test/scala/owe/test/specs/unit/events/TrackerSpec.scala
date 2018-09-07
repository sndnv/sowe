package owe.test.specs.unit.events

import akka.actor.ActorRef
import org.scalatest.Outcome
import owe.events.Event.SystemEvent
import owe.events.Tracker._
import owe.events.{Event, Tracker}
import owe.map.grid.Point
import owe.test.specs.unit.AkkaUnitSpec

class TrackerSpec extends AkkaUnitSpec("TrackerSpec") {

  case object TestEvent1 extends Event.Game
  case object TestEvent2 extends Event.Game
  case object TestEvent3 extends Event.Game

  case class TestEvent(id: Event.Identifier, targetCell: Option[Point]) extends Event

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  private val tracker: ActorRef = system.actorOf(Tracker.props())

  "A Tracker" should "attach observers" in { _ =>
    val events = Seq(
      Event.Engine.EntityCreated,
      Event.Engine.EntityDestroyed,
      Event.Engine.EntityMoved,
      Event.Engine.CellsUnavailable,
      Event.Engine.CellOutOfBounds,
      Event.Engine.EntityMissing,
      Event.Engine.DestinationUnreachable,
      TestEvent1,
      TestEvent2,
      TestEvent3
    )

    tracker ! AttachEventsObserver(testActor, events: _*)
    expectMsg(EventsObserverAttached(events: _*))
  }

  it should "forward events to observers" in { _ =>
    val event1 = SystemEvent(id = Event.Engine.EntityMissing)
    val event2 = TestEvent(id = TestEvent2, targetCell = Some(Point(0, 0)))
    val event3 = TestEvent(id = TestEvent1, targetCell = Some(Point(5, 1)))

    tracker ! event1
    tracker ! event2
    tracker ! event3

    expectMsg(event1)
    expectMsg(event2)
    expectMsg(event3)
  }

  it should "allow event querying" in { _ =>
    val event2 = TestEvent(id = TestEvent2, targetCell = Some(Point(0, 0)))
    val event3 = TestEvent(id = TestEvent1, targetCell = Some(Point(5, 1)))

    tracker ! GetGameEventsLog()
    expectMsg(Vector(event2, event3))
  }

  it should "allow event clearing" in { _ =>
    tracker ! ClearGameEventsLog()

    tracker ! GetGameEventsLog()
    expectMsg(Vector.empty[Event])
  }

  it should "detach observers" in { _ =>
    val events = Seq(
      Event.Engine.CellsUnavailable,
      Event.Engine.CellOutOfBounds,
      Event.Engine.EntityMissing
    )

    tracker ! DetachEventsObserver(testActor, events: _*)
    expectMsg(EventsObserverDetached(events: _*))

    val detachedEvent = TestEvent(id = Event.Engine.EntityMissing, targetCell = None)
    tracker ! detachedEvent
    expectNoMessage()

    val attachedEvent = TestEvent(id = Event.Engine.DestinationUnreachable, targetCell = None)
    tracker ! attachedEvent
    expectMsg(attachedEvent)
  }
}
