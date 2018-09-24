package owe.test.specs.unit.map.ops

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.Tagging._
import owe.entities.Entity
import owe.entities.Entity.EntityRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Doodad
import owe.entities.passive.Road.RoadRef
import owe.map.Cell._
import owe.map.grid.{Grid, Point}
import owe.map.ops.AvailabilityOps
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AsyncUnitSpec

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class AvailabilityOpsSpec extends AsyncUnitSpec {
  private class Ops extends AvailabilityOps {
    override protected implicit val actionTimeout: Timeout = 3.seconds
    override protected implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  }

  private class TestActor(hasRoad: Boolean = true, hasRoadblock: Boolean = true) extends Actor {
    override def receive: Receive = {
      case GetCellAvailability() =>
        val data = CellData.empty

        val entityTypes: Set[Entity.Type] = Set(
          if (hasRoad) { Some(Entity.Type.Road) } else None,
          if (hasRoadblock) { Some(Entity.Type.Roadblock) } else None
        ).flatten

        sender ! Availability(
          data.`type`,
          data.state,
          entityTypes
        )

      case GetEntity(entityID) =>
        entityID match {
          case _: RoadRef =>
            sender ! Some(
              MapEntity(
                entityRef = entityID,
                parentCell = Point(0, 1),
                spec = new Doodad
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

  private implicit class OptionalFuture[T](optionalFuture: Option[Future[T]]) {
    def toFutureOption: Future[Option[T]] = optionalFuture match {
      case Some(future) => future.map(t => Some(t))
      case None         => Future.successful(None)
    }
  }

  "Availability ops" should "calculate cell availability" in { fixture =>
    val cellRef = system.actorOf(Props(new TestActor(hasRoad = false, hasRoadblock = false))).tag[Cell.ActorRefTag]

    for {
      cellResult <- fixture.ops.cellAvailability(cellRef)
      gridResult <- fixture.ops.cellAvailability(fixture.grid, (0, 0)).toFutureOption
      gridResultOutOfBounds <- fixture.ops.cellAvailability(fixture.grid, (5, 5)).toFutureOption
      hasRoad <- fixture.ops.cellAvailability(cellRef).map(_.hasRoad)
      hasRoadblock <- fixture.ops.cellAvailability(cellRef).map(_.hasRoadblock)
    } yield {
      cellResult.isFree should be(true)
      gridResult.getOrElse(fail("Unexpected response received")).isFree should be(false)
      gridResultOutOfBounds should be(None)
      hasRoad should be(false)
      hasRoadblock should be(false)
    }
  }

  they should "check if a cell has a roadblock" in { fixture =>
    val cellWithRoadblockRef = system.actorOf(Props(new TestActor(hasRoadblock = true))).tag[Cell.ActorRefTag]
    val cellWithoutRoadblockRef = system.actorOf(Props(new TestActor(hasRoadblock = false))).tag[Cell.ActorRefTag]

    for {
      withRoadblockResult <- fixture.ops.cellAvailability(cellWithRoadblockRef).map(_.hasRoadblock)
      withoutRoadblockResult <- fixture.ops.cellAvailability(cellWithoutRoadblockRef).map(_.hasRoadblock)
    } yield {
      withRoadblockResult should be(true)
      withoutRoadblockResult should be(false)
    }
  }

  they should "check if a cell has a road" in { fixture =>
    val cellWithRoadRef = system.actorOf(Props(new TestActor(hasRoad = true))).tag[Cell.ActorRefTag]
    val cellWithoutRoadRef = system.actorOf(Props(new TestActor(hasRoad = false))).tag[Cell.ActorRefTag]

    for {
      withRoadResult <- fixture.ops.cellAvailability(cellWithRoadRef).map(_.hasRoad)
      withoutRoadResult <- fixture.ops.cellAvailability(cellWithoutRoadRef).map(_.hasRoad)
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
          spec = new Doodad
        )
      )
    }

    for {
      missingEntityResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        missingEntityID,
        _.hasRoad
      )
      missingCellResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        missingCellEntityID,
        _.hasRoad
      )
      missingMapEntityResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        missingMapEntityID,
        _.hasRoad
      )
      existingEntityResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        entityID,
        _.hasRoad
      )
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
          spec = new Doodad
        )
      )
    }

    for {
      missingEntityResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        missingEntityID,
        _.isPassable
      )
      missingCellResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        missingCellEntityID,
        _.isPassable
      )
      missingMapEntityResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        missingMapEntityID,
        _.isPassable
      )
      existingEntityResult <- fixture.ops.findFirstAdjacentPoint(
        fixture.grid,
        entities,
        entityID,
        _.isPassable
      )
    } yield {
      missingEntityResult should be(None)
      missingCellResult should be(None)
      missingMapEntityResult should be(None)
      // because TestActor always returns cell availability as buildable
      existingEntityResult should be(Some(Point(0, 0)))
    }
  }

  they should "check if an entity is allowed to traverse an empty cell" in { fixture =>
    val defaultAvailability = Availability(
      cellType = Cell.Type.Land,
      cellState = CellData.empty.state,
      entityTypes = Set.empty
    )

    // on land, without roadblock restriction
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = false,
      defaultAvailability
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = false,
      defaultAvailability
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnLand,
      roadblockRestricted = false,
      defaultAvailability
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnWater,
      roadblockRestricted = false,
      defaultAvailability
    ) should be(false)

    // on land, with roadblock restriction
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = true,
      defaultAvailability
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = true,
      defaultAvailability
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnLand,
      roadblockRestricted = true,
      defaultAvailability
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnWater,
      roadblockRestricted = true,
      defaultAvailability
    ) should be(false)

    // on water
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Water)
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Water)
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnLand,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Water)
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnWater,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Water)
    ) should be(true)

    // on beach
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Beach)
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Beach)
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnLand,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Beach)
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnWater,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Beach)
    ) should be(false)

    // on floodplain, without roadblock restriction
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Floodplain)
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnLand,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnWater,
      roadblockRestricted = false,
      defaultAvailability.copy(cellType = Cell.Type.Floodplain)
    ) should be(false)

    // on floodplain, with roadblock restriction
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = true,
      defaultAvailability.copy(cellType = Cell.Type.Floodplain)
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = true,
      defaultAvailability.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnLand,
      roadblockRestricted = true,
      defaultAvailability.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.OnWater,
      roadblockRestricted = true,
      defaultAvailability.copy(cellType = Cell.Type.Floodplain)
    ) should be(false)
  }

  they should "check if an entity is allowed to traverse a cell with entities" in { fixture =>
    val availabilityWithRoad = Availability(
      cellType = Cell.Type.Land,
      cellState = CellData.empty.state,
      entityTypes = Set(Entity.Type.Road)
    )

    val availabilityWithRoadblock = Availability(
      cellType = Cell.Type.Land,
      cellState = CellData.empty.state,
      entityTypes = Set(Entity.Type.Road, Entity.Type.Roadblock)
    )

    // on land, without roadblock restriction
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = false,
      availabilityWithRoad
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = false,
      availabilityWithRoad
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = false,
      availabilityWithRoadblock
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = false,
      availabilityWithRoadblock
    ) should be(true)

    // on land, with roadblock restriction
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = true,
      availabilityWithRoad
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = true,
      availabilityWithRoad
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = true,
      availabilityWithRoadblock
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = true,
      availabilityWithRoadblock
    ) should be(true)

    // on floodplain, without roadblock restriction
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = false,
      availabilityWithRoad.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = false,
      availabilityWithRoad.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = false,
      availabilityWithRoadblock.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = false,
      availabilityWithRoadblock.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    // on floodplain, with roadblock restriction
    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = true,
      availabilityWithRoad.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = true,
      availabilityWithRoad.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadRequired,
      roadblockRestricted = true,
      availabilityWithRoadblock.copy(cellType = Cell.Type.Floodplain)
    ) should be(false)

    fixture.ops.isTraversable(
      traversalMode = Walker.TraversalMode.RoadPreferred,
      roadblockRestricted = true,
      availabilityWithRoadblock.copy(cellType = Cell.Type.Floodplain)
    ) should be(true)
  }
}
