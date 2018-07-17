package owe.test.specs.unit.entities.active.behaviour.walker

import akka.actor.Props
import akka.testkit.TestProbe
import akka.util.Timeout
import owe.entities.active.Walker
import owe.entities.active.Walker.WalkerRef
import owe.events.Event
import owe.map.GameMap.CreateEntity
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.map.TestGameMap
import owe.test.specs.unit.map.TestGameMap.StartBehaviour

import scala.concurrent.duration._

trait WalkerBehaviour { _: AkkaUnitSpec =>
  private implicit val timeout: Timeout = 5.seconds

  def roamingWalker(walker: Walker): Unit =
    it should "roam around" in { _ =>
      val testProbe = TestProbe()
      val map = system.actorOf(
        Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 500.millis))
      )

      map.tell(CreateEntity(walker, (0, 0)), testProbe.ref)
      testProbe.expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))
      testProbe.expectMsgType[WalkerRef]

      val (ticks, moved) = testProbe
        .receiveWhile(timeout.duration) {
          case Event(Event.System.TickProcessed, None) => (1, 0)
          case Event(Event.System.EntityMoved, _)      => (0, 1)
        }
        .foldLeft((0, 0)) {
          case ((totalTicks, totalMoved), (currentTick, currentMoved)) =>
            (totalTicks + currentTick, totalMoved + currentMoved)
        }

      ticks should be > 0
      moved should be > 0
    }

  def attackingWalker(walker: Walker, enemy: Walker): Unit =
    it should "attack if any enemies are in range" in { _ =>
      val testProbe = TestProbe()
      val map = system.actorOf(
        Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 500.millis))
      )

      map.tell(CreateEntity(walker, (0, 0)), testProbe.ref)
      testProbe.expectMsg(Event(Event.System.EntityCreated, Some((0, 0))))
      testProbe.expectMsgType[WalkerRef]

      map.tell(CreateEntity(enemy, (1, 1)), testProbe.ref)
      testProbe.expectMsg(Event(Event.System.EntityCreated, Some((1, 1))))
      testProbe.expectMsgType[WalkerRef]

      val (ticks, destroyed) = testProbe
        .receiveWhile(timeout.duration) {
          case Event(Event.System.TickProcessed, None) => (1, 0)
          case Event(Event.System.EntityDestroyed, _)  => (0, 1)
          case _                                       => (0, 0)
        }
        .foldLeft((0, 0)) {
          case ((totalTicks, totalDestroyed), (currentTick, currentDestroyed)) =>
            (totalTicks + currentTick, totalDestroyed + currentDestroyed)
        }

      ticks should be > 0
      destroyed should be > 0
    }

  def returningWalker(walker: Walker): Unit =
    it should "return home when max distance is covered" in { _ =>
      fail("Not Implemented", new NotImplementedError())
    }

  def followingWalker(walker: Walker): Unit =
    it should "follow another walker" in { _ =>
      fail("Not Implemented", new NotImplementedError())
    }

  def actingWalker(walker: Walker): Unit =
    it should "go to a destination and perform action" in { _ =>
      fail("Not Implemented", new NotImplementedError())
    }
}
