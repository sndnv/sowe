package owe.test.specs.unit.map.ops

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.map.{Cell, MapEntity}
import owe.map.Cell._
import owe.map.ops.AvailabilityOps
import owe.test.specs.unit.AsyncUnitSpec
import owe.Tagging._
import owe.entities.Entity
import owe.entities.Entity.{Desirability, EntityRef}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Road.RoadRef
import owe.map.grid.{Grid, Point}

class AvailabilityOpsSpec extends AsyncUnitSpec {
  private class Ops extends AvailabilityOps {
    override protected implicit val actionTimeout: Timeout = 3.seconds
    override protected implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  }

  private class TestActor extends Actor {
    override def receive: Receive = {
      case GetCellAvailability() => sender ! Cell.Availability.Buildable
      case HasRoad()             => sender ! true
      case GetEntity(entityID) =>
        entityID match {
          case _: RoadRef =>
            sender ! Some(
              MapEntity(
                entityRef = entityID,
                parentCell = Point(0, 1),
                size = Entity.Size(1, 1),
                desirability = Desirability.Min
              )
            )

          case _ =>
            sender ! None
        }
    }
  }

  private implicit val system: ActorSystem = ActorSystem()

  case class FixtureParam(ops: AvailabilityOps, grid: Grid[CellActorRef])

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withFixture(
      test.toNoArgAsyncTest(
        FixtureParam(
          ops = new Ops,
          grid = Grid[CellActorRef](
            size = 3,
            f = system.actorOf(Props(new TestActor)).tag[Cell.ActorRefTag]
          )
        )
      )
    )

  "Availability ops" should "calculate cell availability" in { fixture =>
    val cellRef = system.actorOf(Props(new TestActor)).tag[Cell.ActorRefTag]

    for {
      cellResult <- fixture.ops.cellAvailability(cellRef)
      gridResult <- fixture.ops.cellAvailability(fixture.grid, (0, 0))
      gridResultOutOfBounds <- fixture.ops.cellAvailability(fixture.grid, (5, 5))
      hasRoad <- fixture.ops.cellHasRoad(cellRef)
    } yield {
      cellResult should be(Cell.Availability.Buildable)
      gridResult should be(Cell.Availability.Buildable)
      gridResultOutOfBounds should be(Cell.Availability.OutOfBounds)
      hasRoad should be(true)
    }
  }

  they should "find adjacent roads" in { fixture =>
    val missingEntityID = StructureRef(TestProbe().ref)
    val missingCellEntityID = StructureRef(TestProbe().ref)
    val missingMapEntityID = WalkerRef(TestProbe().ref)
    val entityID = RoadRef(TestProbe().ref)

    val entities = Map[EntityRef, Point](
      entityID -> Point(0, 1),
      missingCellEntityID -> Point(5, 5),
      missingMapEntityID -> Point(0, 0)
    )

    fixture.grid.get((0, 1)).foreach { ref =>
      ref ! AddEntity(
        MapEntity(
          entityRef = entityID,
          parentCell = Point(0, 1),
          size = Entity.Size(1, 1),
          desirability = Desirability.Min
        )
      )
    }

    for {
      missingEntityResult <- fixture.ops.findFirstAdjacentRoad(fixture.grid, entities, missingEntityID)
      missingCellResult <- fixture.ops.findFirstAdjacentRoad(fixture.grid, entities, missingCellEntityID)
      missingMapEntityResult <- fixture.ops.findFirstAdjacentRoad(fixture.grid, entities, missingMapEntityID)
      existingEntityResult <- fixture.ops.findFirstAdjacentRoad(fixture.grid, entities, entityID)
    } yield {
      missingEntityResult should be(None)
      missingCellResult should be(None)
      missingMapEntityResult should be(None)
      existingEntityResult should be(Some(Point(0, 0))) // because TestActor always returns 'true' for 'HasRoad'
    }
  }
}
