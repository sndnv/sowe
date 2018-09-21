package owe.test.specs.unit.map.ops

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.Tagging._
import owe.entities.Entity
import owe.entities.Entity.{Desirability, EntityRef}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Road.RoadRef
import owe.map.Cell._
import owe.map.grid.{Grid, Point}
import owe.map.ops.AvailabilityOps
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AsyncUnitSpec

class AvailabilityOpsSpec extends AsyncUnitSpec {
  private class Ops extends AvailabilityOps {
    override protected implicit val actionTimeout: Timeout = 3.seconds
    override protected implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  }

  private class TestActor(hasRoad: Boolean = true, hasRoadblock: Boolean = true) extends Actor {
    override def receive: Receive = {
      case GetCellAvailability() => sender ! Cell.Availability.Buildable
      case HasRoad()             => sender ! hasRoad
      case HasRoadblock()        => sender ! hasRoadblock
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
  implicit val sender: ActorRef = Actor.noSender

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
      gridResult <- fixture.ops.cellAvailabilityForPoint(fixture.grid, (0, 0))
      gridResultOutOfBounds <- fixture.ops.cellAvailabilityForPoint(fixture.grid, (5, 5))
      hasRoad <- fixture.ops.cellHasRoad(cellRef)
    } yield {
      cellResult should be(Cell.Availability.Buildable)
      gridResult should be(Cell.Availability.Buildable)
      gridResultOutOfBounds should be(Cell.Availability.OutOfBounds)
      hasRoad should be(true)
    }
  }

  they should "check if a cell has a roadblock" in { fixture =>
    val cellWithRoadblockRef = system.actorOf(Props(new TestActor(hasRoadblock = true))).tag[Cell.ActorRefTag]
    val cellWithoutRoadblockRef = system.actorOf(Props(new TestActor(hasRoadblock = false))).tag[Cell.ActorRefTag]

    for {
      withRoadblockResult <- fixture.ops.cellHasRoadblock(cellWithRoadblockRef)
      withoutRoadblockResult <- fixture.ops.cellHasRoadblock(cellWithoutRoadblockRef)
    } yield {
      withRoadblockResult should be(true)
      withoutRoadblockResult should be(false)
    }
  }

  they should "check if a cell has a road" in { fixture =>
    val cellWithRoadRef = system.actorOf(Props(new TestActor(hasRoad = true))).tag[Cell.ActorRefTag]
    val cellWithoutRoadRef = system.actorOf(Props(new TestActor(hasRoad = false))).tag[Cell.ActorRefTag]

    for {
      withRoadResult <- fixture.ops.cellHasRoad(cellWithRoadRef)
      withoutRoadResult <- fixture.ops.cellHasRoad(cellWithoutRoadRef)
    } yield {
      withRoadResult should be(true)
      withoutRoadResult should be(false)
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

  they should "find adjacent points" in { fixture =>
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
      missingEntityResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        missingEntityID,
        Availability.Passable
      )
      missingCellResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        missingCellEntityID,
        Availability.Passable
      )
      missingMapEntityResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        missingMapEntityID,
        Availability.Passable
      )
      existingEntityResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        entityID,
        Availability.Passable
      )
    } yield {
      missingEntityResult should be(None)
      missingCellResult should be(None)
      missingMapEntityResult should be(None)
      // because TestActor always returns cell availability as buildable
      existingEntityResult should be(Some(Point(0, 0)))
    }
  }
}
