package owe.test.specs.unit.map.ops

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.Tagging._
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.TraversalMode
import owe.entities.active.attributes.Distance
import owe.map.Cell.{AddEntity, CellActorRef}
import owe.map.grid.{Grid, Point}
import owe.map.ops.{AvailabilityOps, PathfindingOps}
import owe.map.pathfinding.{DepthFirstSearch, Search}
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AsyncUnitSpec
import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import owe.entities.passive.Roadblock.RoadblockRef

class PathfindingOpsSpec extends AsyncUnitSpec {
  private implicit val timeout: Timeout = 3.seconds

  private class Ops extends PathfindingOps with AvailabilityOps {
    override protected implicit val actionTimeout: Timeout = timeout
    override protected implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
    override protected val search: Search = DepthFirstSearch
  }

  private implicit val system: ActorSystem = ActorSystem()

  private val existingStructureMapEntity = MapEntity(
    entityRef = StructureRef(TestProbe().ref),
    parentCell = Point(1, 1),
    size = Entity.Size(2, 1),
    desirability = Desirability.Min
  )

  private val existingRoadblockMapEntity = MapEntity(
    entityRef = RoadblockRef(TestProbe().ref),
    parentCell = Point(1, 0),
    size = Entity.Size(1, 1),
    desirability = Desirability.Min
  )

  case class FixtureParam(ops: PathfindingOps, grid: Grid[CellActorRef])

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

  "Pathfinding ops" should "generate advance paths" in { fixture =>
    fixture.grid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity)
    fixture.grid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity)

    for {
      invalidPath <- fixture.ops.generateAdvancePath(fixture.grid, (0, 1), (13, 5), TraversalMode.OnLand)
      validPath <- fixture.ops.generateAdvancePath(fixture.grid, (0, 1), (2, 2), TraversalMode.OnLand)
    } yield {
      invalidPath should be(Queue.empty[Point])
      validPath should be(Queue[Point]((1, 0), (2, 1), (2, 2)))
    }
  }

  they should "generate roaming paths" in { fixture =>
    fixture.grid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity)
    fixture.grid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity)

    for {
      atMaxDistancePath <- fixture.ops.generateRoamPath(fixture.grid, (0, 1), Distance(4), TraversalMode.OnLand)
      belowMaxDistancePath1 <- fixture.ops.generateRoamPath(fixture.grid, (0, 1), Distance(10), TraversalMode.OnLand)
      belowMaxDistancePath2 <- fixture.ops.generateRoamPath(fixture.grid, (0, 1), Distance(42), TraversalMode.OnLand)
    } yield {
      atMaxDistancePath should be(Queue[Point]((0, 1), (0, 0), (1, 0), (2, 0)))
      belowMaxDistancePath1 should be(Queue[Point]((0, 1), (0, 0), (1, 0), (2, 0), (2, 1), (2, 2)))
      belowMaxDistancePath2 should be(belowMaxDistancePath1)
    }
  }

  they should "calculate passable neighbours" in { fixture =>
    fixture.grid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity)
    fixture.grid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity)
    fixture.grid.getUnsafe((1, 0)) ! AddEntity(existingRoadblockMapEntity)

    for {
      neighboursOf00 <- fixture.ops.passableNeighboursOf(
        fixture.grid,
        (0, 0),
        TraversalMode.OnLand,
        roadblockRestricted = false
      )
      neighboursOf01 <- fixture.ops.passableNeighboursOf(
        fixture.grid,
        (0, 1),
        TraversalMode.OnLand,
        roadblockRestricted = false
      )
      neighboursOf22 <- fixture.ops.passableNeighboursOf(
        fixture.grid,
        (2, 2),
        TraversalMode.OnLand,
        roadblockRestricted = false
      )
      neighboursOf00WithRoadblockRestriction <- fixture.ops.passableNeighboursOf(
        fixture.grid,
        (0, 0),
        TraversalMode.OnLand,
        roadblockRestricted = true
      )
      neighboursOf01WithRoadblockRestriction <- fixture.ops.passableNeighboursOf(
        fixture.grid,
        (0, 1),
        TraversalMode.OnLand,
        roadblockRestricted = true
      )
      neighboursOf22WithRoadblockRestriction <- fixture.ops.passableNeighboursOf(
        fixture.grid,
        (2, 2),
        TraversalMode.OnLand,
        roadblockRestricted = true
      )
      neighboursOf20WithoutCornerNeighbours <- fixture.ops.passableNeighboursOf(
        fixture.grid,
        (2, 0),
        TraversalMode.RoadRequired,
        roadblockRestricted = false
      )
      neighboursOf21WithoutCornerNeighbours <- fixture.ops.passableNeighboursOf(
        fixture.grid,
        (2, 1),
        TraversalMode.RoadRequired,
        roadblockRestricted = false
      )
    } yield {
      neighboursOf00 should contain.theSameElementsAs(Seq[Point]((1, 0), (0, 1)))
      neighboursOf01 should contain.theSameElementsAs(Seq[Point]((0, 0), (1, 0), (0, 2)))
      neighboursOf22 should contain.theSameElementsAs(Seq[Point]((2, 1)))
      neighboursOf00WithRoadblockRestriction should contain.theSameElementsAs(Seq[Point]((0, 1)))
      neighboursOf01WithRoadblockRestriction should contain.theSameElementsAs(Seq[Point]((0, 0), (0, 2)))
      neighboursOf22WithRoadblockRestriction should contain.theSameElementsAs(Seq[Point]((2, 1)))
      neighboursOf20WithoutCornerNeighbours should contain.theSameElementsAs(Seq[Point]((1, 0), (2, 1)))
      neighboursOf21WithoutCornerNeighbours should contain.theSameElementsAs(Seq[Point]((2, 0), (2, 2)))
    }
  }

  they should "calculate diagonal movement restrictions" in { fixture =>
    fixture.ops.diagonalMovementAllowed(TraversalMode.RoadRequired) should be(false)
    fixture.ops.diagonalMovementAllowed(TraversalMode.RoadPreferred) should be(true)
    fixture.ops.diagonalMovementAllowed(TraversalMode.OnLand) should be(true)
    fixture.ops.diagonalMovementAllowed(TraversalMode.OnWater) should be(true)
  }
}
