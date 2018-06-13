package owe.test.specs.unit.events

import akka.actor.ActorRef
import org.scalatest.Outcome
import owe.events.Tracker._
import owe.events.{Event, Tracker}
import owe.map.grid.Point
import owe.test.specs.unit.AkkaUnitSpec

class TrackerSpec extends AkkaUnitSpec("TrackerSpec") {

  case object TestEvent1 extends Event.Game
  case object TestEvent2 extends Event.Game
  case object TestEvent3 extends Event.Game

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  private val tracker: ActorRef = system.actorOf(Tracker.props())

  "A Tracker" should "attach observers" in { fixture =>
    val events = Seq(
      Event.System.EntityCreated,
      Event.System.EntityDestroyed,
      Event.System.EntityMoved,
      Event.System.CellsUnavailable,
      Event.System.CellOutOfBounds,
      Event.System.EntityMissing,
      Event.System.DestinationUnreachable,
      TestEvent1,
      TestEvent2,
      TestEvent3
    )

    tracker ! AttachEventsObserver(testActor, events: _*)
    expectMsg(EventsObserverAttached(events: _*))
  }

  it should "forward events to observers" in { fixture =>
    val event1 = Event(id = Event.System.EntityCreated, cell = None)
    val event2 = Event(id = TestEvent2, cell = Some(Point(0, 0)))
    val event3 = Event(id = TestEvent1, cell = Some(Point(5, 1)))

    tracker ! event1
    tracker ! event2
    tracker ! event3

    expectMsg(event1)
    expectMsg(event2)
    expectMsg(event3)
  }

  it should "allow event querying" in { fixture =>
    val event2 = Event(id = TestEvent2, cell = Some(Point(0, 0)))
    val event3 = Event(id = TestEvent1, cell = Some(Point(5, 1)))

    tracker ! GetGameEventsLog()
    expectMsg(Vector(event2, event3))
  }

  it should "allow event clearing" in { fixture =>
    tracker ! ClearGameEventsLog()

    tracker ! GetGameEventsLog()
    expectMsg(Vector.empty[Event])
  }

  it should "detach observers" in { fixture =>
    val events = Seq(
      Event.System.CellsUnavailable,
      Event.System.CellOutOfBounds,
      Event.System.EntityMissing
    )

    tracker ! DetachEventsObserver(testActor, events: _*)
    expectMsg(EventsObserverDetached(events: _*))

    val detachedEvent = Event(id = Event.System.EntityMissing, cell = None)
    tracker ! detachedEvent
    expectNoMessage()

    val attachedEvent = Event(id = Event.System.DestinationUnreachable, cell = None)
    tracker ! attachedEvent
    expectMsg(attachedEvent)
  }
}
