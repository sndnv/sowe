package owe.test.specs.unit.entities.active.behaviour.walker

import akka.actor.Props
import akka.testkit.TestProbe
import akka.util.Timeout
import owe.entities.ActiveEntityActor.AddEntityInstruction
import owe.entities.active.Walker
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.events.Event
import owe.map.GameMap.CreateEntity
import owe.map.grid.Point
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.map.TestGameMap
import owe.test.specs.unit.map.TestGameMap.StartBehaviour

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import owe.events.Event.{EntityEvent, SystemEvent}
import owe.production.Exchange.{AddConsumer, AddProducer}
import owe.test.specs.unit.entities.EntityTestHelpers

trait WalkerBehaviour extends EntityTestHelpers { _: AkkaUnitSpec =>
  protected implicit val timeout: Timeout = 5.seconds

  def roamingWalker(walker: Walker): Unit =
    it should "roam around" in { _ =>
      val testProbe = TestProbe()
      testProbe.ignoreMsg {
        case _: AddProducer => true
        case _: AddConsumer => true
      }

      val map = system.actorOf(
        Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 500.millis))
      )

      val walkerPoint = Point(0, 0)
      map.tell(CreateEntity(walker, walkerPoint), testProbe.ref)
      testProbe.expectEntityCreatedAt(walkerPoint)
      testProbe.expectMsgType[WalkerRef]

      val (ticks, moved) = testProbe
        .receiveWhile(timeout.duration) {
          case SystemEvent(Event.Engine.TickProcessed)     => (1, 0)
          case EntityEvent(Event.Engine.EntityMoved, _, _) => (0, 1)
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
      testProbe.ignoreMsg {
        case _: AddProducer => true
        case _: AddConsumer => true
      }

      val map = system.actorOf(
        Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 500.millis))
      )

      val walkerPoint = Point(0, 0)
      val enemyPoint = Point(1, 1)

      map.tell(CreateEntity(walker, walkerPoint), testProbe.ref)
      testProbe.expectEntityCreatedAt(walkerPoint)
      testProbe.expectMsgType[WalkerRef]

      map.tell(CreateEntity(enemy, enemyPoint), testProbe.ref)
      testProbe.expectEntityCreatedAt(enemyPoint)
      testProbe.expectMsgType[WalkerRef]

      val (ticks, destroyed) = testProbe
        .receiveWhile(timeout.duration) {
          case SystemEvent(Event.Engine.TickProcessed)         => (1, 0)
          case EntityEvent(Event.Engine.EntityDestroyed, _, _) => (0, 1)
          case _                                               => (0, 0)
        }
        .foldLeft((0, 0)) {
          case ((totalTicks, totalDestroyed), (currentTick, currentDestroyed)) =>
            (totalTicks + currentTick, totalDestroyed + currentDestroyed)
        }

      ticks should be > 0
      destroyed should be > 0
    }

  def returningWalker(walker: Walker, homePosition: Point): Unit =
    it should "return home when max distance is covered" in { _ =>
      val testProbe = TestProbe()
      testProbe.ignoreMsg {
        case _: AddProducer => true
        case _: AddConsumer => true
      }

      val map = system.actorOf(
        Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 500.millis))
      )

      val walkerPoint = Point(2, 2)

      map.tell(CreateEntity(walker, walkerPoint), testProbe.ref)
      testProbe.expectEntityCreatedAt(walkerPoint)
      testProbe.expectMsgType[WalkerRef]

      val (ticks, moved) = testProbe
        .receiveWhile(timeout.duration) {
          case SystemEvent(Event.Engine.TickProcessed)                  => (1, 0)
          case EntityEvent(Event.Engine.EntityMoved, _, `homePosition`) => (0, 1)
          case _                                                        => (0, 0)
        }
        .foldLeft((0, 0)) {
          case ((totalTicks, totalMoved), (currentTick, currentMoved)) =>
            (totalTicks + currentTick, totalMoved + currentMoved)
        }

      ticks should be > 0
      moved should be(1)
    }

  def followingWalker(
    followingWalker: Walker,
    followedWalker: Walker,
    expectedFollowedPath: Queue[Point]
  ): Unit =
    followingBehaviour(
      behave = "follow another walker",
      followingWalker,
      followedWalker,
      expectedFollowedPath
    )

  def followedWalker(
    followedWalker: Walker,
    followingWalker: Walker,
    expectedFollowedPath: Queue[Point]
  ): Unit =
    followingBehaviour(
      behave = "be followed by another walker",
      followingWalker,
      followedWalker,
      expectedFollowedPath
    )

  private def followingBehaviour(
    behave: String,
    followingWalker: Walker,
    followedWalker: Walker,
    expectedFollowedPath: Queue[Point]
  ): Unit =
    it should behave in { _ =>
      val testProbe = TestProbe()
      testProbe.ignoreMsg {
        case _: AddProducer => true
        case _: AddConsumer => true
      }

      val map = system.actorOf(
        Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 500.millis))
      )

      val followingWalkerPoint = Point(0, 0)
      val followedWalkerPoint = Point(1, 0)

      map.tell(CreateEntity(followingWalker, followingWalkerPoint), testProbe.ref)
      testProbe.expectEntityCreatedAt(followingWalkerPoint)
      val followingWalkerRef = testProbe.receiveOne(timeout.duration).asInstanceOf[WalkerRef]

      map.tell(CreateEntity(followedWalker, followedWalkerPoint), testProbe.ref)
      testProbe.expectEntityCreatedAt(followedWalkerPoint)
      val followedWalkerRef = testProbe.receiveOne(timeout.duration).asInstanceOf[WalkerRef]

      followingWalkerRef ! AddEntityInstruction(
        DoTransition(
          Advance(destination = DestinationEntity(followedWalkerRef), destinationActions = Seq.empty)
        )
      )

      val (ticks, positions) = testProbe
        .receiveWhile(timeout.duration) {
          case SystemEvent(Event.Engine.TickProcessed)            => (1, None)
          case EntityEvent(Event.Engine.EntityMoved, _, position) => (0, Some(position))
          case _                                                  => (0, None)
        }
        .foldLeft((0, Seq.empty[Point])) {
          case ((totalTicks, allPositions), (currentTick, currentPosition)) =>
            (
              totalTicks + currentTick,
              currentPosition match {
                case Some(position) => allPositions :+ position
                case None           => allPositions
              }
            )
        }

      ticks should be > 0

      val expectedFollowingPath = followedWalkerPoint +: expectedFollowedPath

      positions.size should be(expectedFollowingPath.size + expectedFollowedPath.size)
      (positions should contain).theSameElementsAs(expectedFollowingPath ++ expectedFollowedPath)
    }

  def advancingWalker(walker: Walker, destination: Point, action: Action): Unit =
    it should "go to a destination and perform action" in { _ =>
      val testProbe = TestProbe()
      testProbe.ignoreMsg {
        case _: AddProducer => true
        case _: AddConsumer => true
      }

      val map = system.actorOf(
        Props(new TestGameMap(testProbe.ref, StartBehaviour.Idle, interval = 500.millis))
      )

      val walkerPoint = Point(2, 0)

      map.tell(CreateEntity(walker, walkerPoint), testProbe.ref)
      testProbe.expectEntityCreatedAt(walkerPoint)
      testProbe.expectMsgType[WalkerRef]

      val (ticks, moved) = testProbe
        .receiveWhile(timeout.duration) {
          case SystemEvent(Event.Engine.TickProcessed)                 => (1, 0)
          case EntityEvent(Event.Engine.EntityMoved, _, `destination`) => (0, 1)
          case event =>
            action match {
              case GoToPoint(secondDestination) =>
                event match {
                  case EntityEvent(Event.Engine.EntityMoved, _, `secondDestination`) => (0, 1)
                  case _                                                             => (0, 0)
                }

              case _ => (0, 0)
            }
        }
        .foldLeft((0, 0)) {
          case ((totalTicks, totalMoved), (currentTick, currentMoved)) =>
            (totalTicks + currentTick, totalMoved + currentMoved)
        }

      ticks should be > 0

      action match {
        case GoToPoint(_) =>
          moved should be(2)

        case Idle() =>
          moved should be(1)

        case _ =>
          fail(s"Unexpected action specified: [$action]")
      }

    }
}
