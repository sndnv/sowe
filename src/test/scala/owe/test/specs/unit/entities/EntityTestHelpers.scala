package owe.test.specs.unit.entities

import akka.testkit.TestKit
import akka.util.Timeout
import org.scalatest.Assertions
import owe.events.Event
import owe.events.Event.EntityEvent
import owe.map.grid.Point

trait EntityTestHelpers { _: Assertions =>
  protected implicit val timeout: Timeout

  implicit class RichTestKit(testKit: TestKit) {
    def expectEntityCreatedAt(point: Point): Unit = expectEntityEventAt(Event.Engine.EntityCreated, point)

    def expectEntityDestroyedAt(point: Point): Unit = expectEntityEventAt(Event.Engine.EntityDestroyed, point)

    def expectEntityMovedTo(point: Point): Unit = expectEntityEventAt(Event.Engine.EntityMoved, point)

    def expectEntityEventAt(id: Event.Identifier, point: Point): Unit =
      testKit.receiveOne(timeout.duration) match {
        case EntityEvent(`id`, _, `point`) => ()
        case message                       => fail(s"Unexpected message received: [$message]")
      }
  }
}
