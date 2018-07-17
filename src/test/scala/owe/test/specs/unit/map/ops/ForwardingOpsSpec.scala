package owe.test.specs.unit.map.ops

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.Tagging._
import owe.entities.ActiveEntityActor.AddEntityMessage
import owe.entities.Entity
import owe.entities.Entity.{Desirability, EntityRef, ProcessLabourFound}
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Road.RoadRef
import owe.events.Event
import owe.map.{Cell, MapEntity}
import owe.map.Cell.{AddEntity, CellActorRef}
import owe.map.grid.{Grid, Point}
import owe.map.ops.ForwardingOps
import owe.test.specs.unit.AsyncUnitSpec

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ForwardingOpsSpec extends AsyncUnitSpec {
  private implicit val timeout: Timeout = 3.seconds

  private class Ops extends ForwardingOps {
    override protected implicit val actionTimeout: Timeout = timeout
    override protected implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  }

  private implicit val system: ActorSystem = ActorSystem()

  case class FixtureParam(ops: ForwardingOps, grid: Grid[CellActorRef])

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFixture(
      test.toNoArgAsyncTest(
        FixtureParam(
          ops = new Ops,
          grid = Grid[CellActorRef](
            size = 3,
            f = system.actorOf(Cell.props()).tag[Cell.ActorRefTag]
          )
        )
      )
    )

  "Forwarding ops" should "forward messages" in { fixture =>
    val missingEntityID = WalkerRef(TestProbe().ref)

    val outOfBoundsCell = Point(7, 12)
    val outOfBoundsEntityID = WalkerRef(TestProbe().ref)

    val missingMapEntityCell = Point(2, 2)
    val missingMapEntityID = WalkerRef(TestProbe().ref)

    val roadCell = Point(0, 1)
    val roadEntityID = RoadRef(TestProbe().ref)
    val roadMapEntity = MapEntity(
      entityRef = roadEntityID,
      parentCell = roadCell,
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )

    val walkerTestProbe = TestProbe()
    val walkerCell = Point(1, 1)
    val walkerEntityID = WalkerRef(walkerTestProbe.ref)
    val walkerMapEntity = MapEntity(
      entityRef = walkerEntityID,
      parentCell = walkerCell,
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )

    val message = ProcessLabourFound()

    val entities = Map[EntityRef, Point](
      outOfBoundsEntityID -> outOfBoundsCell,
      missingMapEntityID -> missingMapEntityCell,
      roadEntityID -> roadCell,
      walkerEntityID -> walkerCell
    )
    fixture.grid.getUnsafe(roadCell) ! AddEntity(roadMapEntity)
    fixture.grid.getUnsafe(walkerCell) ! AddEntity(walkerMapEntity)

    for {
      entityNotFoundResult <- fixture.ops.forwardEntityMessage(fixture.grid, entities, missingEntityID, message)
      cellMissingResult <- fixture.ops.forwardEntityMessage(fixture.grid, entities, outOfBoundsEntityID, message)
      mapEntityMissingResult <- fixture.ops.forwardEntityMessage(fixture.grid, entities, missingMapEntityID, message)
      unexpectedEntityResult <- fixture.ops.forwardEntityMessage(fixture.grid, entities, roadEntityID, message)
      successfulResult <- fixture.ops.forwardEntityMessage(fixture.grid, entities, walkerEntityID, message)
    } yield {
      entityNotFoundResult should be(Event(Event.System.EntityMissing, cell = None))
      cellMissingResult should be(Event(Event.System.CellOutOfBounds, Some(outOfBoundsCell)))
      mapEntityMissingResult should be(Event(Event.System.EntityMissing, Some(missingMapEntityCell)))
      unexpectedEntityResult should be(Event(Event.System.UnexpectedEntityFound, Some(roadCell)))
      successfulResult should be(Event(Event.System.MessageForwarded, cell = Some(walkerCell)))

      val entityMessage = walkerTestProbe.receiveOne(timeout.duration).asInstanceOf[AddEntityMessage]
      entityMessage.message should be(ProcessLabourFound())
    }
  }
}
