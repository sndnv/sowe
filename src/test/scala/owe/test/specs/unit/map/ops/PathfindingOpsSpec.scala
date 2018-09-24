package owe.test.specs.unit.map.ops

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.Tagging._
import owe.effects
import owe.entities.Entity.Desirability
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.TraversalMode
import owe.entities.active.attributes.Distance
import owe.entities.active.behaviour.resource.BaseResource
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.{Resource, Structure}
import owe.entities.passive.Road.RoadRef
import owe.entities.passive.Roadblock.RoadblockRef
import owe.entities.passive.{Road, Roadblock}
import owe.entities.{ActiveEntity, Entity}
import owe.map.Cell._
import owe.map.grid.{Grid, Point}
import owe.map.ops.{AvailabilityOps, PathfindingOps}
import owe.map.pathfinding.{AStarSearch, Search}
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AsyncUnitSpec

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class PathfindingOpsSpec extends AsyncUnitSpec {
  private implicit val timeout: Timeout = 3.seconds

  private class Ops extends PathfindingOps with AvailabilityOps {
    override protected implicit val actionTimeout: Timeout = timeout
    override protected implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
    override protected val search: Search = AStarSearch
  }

  private implicit val system: ActorSystem = ActorSystem()

  case class FixtureParam(
    ops: PathfindingOps,
    smallGrid: Grid[CellActorRef],
    largeGrid: Grid[CellActorRef]
  )

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val smallGrid = Grid[CellActorRef](
      size = 3,
      f = system.actorOf(Cell.props()).tag[Cell.ActorRefTag]
    )

    val largeGrid = Grid[CellActorRef](
      size = 9,
      f = system.actorOf(Cell.props()).tag[Cell.ActorRefTag]
    )

    initGrid(smallGrid, baseSmallGridUpdates)
    initGrid(largeGrid, baseLargeGridUpdates)

    withFixture(
      test.toNoArgAsyncTest(
        FixtureParam(
          ops = new Ops,
          smallGrid,
          largeGrid
        )
      )
    )
  }

  "Pathfinding ops" should "generate advance paths" in { fixture =>
    fixture.smallGrid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity)
    fixture.smallGrid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity)

    for {
      invalidPath <- fixture.ops.generateAdvancePath(fixture.smallGrid, (0, 1), (13, 5), TraversalMode.OnLand)
      validPath <- fixture.ops.generateAdvancePath(fixture.smallGrid, (0, 1), (2, 2), TraversalMode.OnLand)
    } yield {
      invalidPath should be(Queue.empty[Point])
      validPath should be(Queue[Point]((1, 0), (2, 1), (2, 2)))
    }
  }

  they should "generate roaming paths" in { fixture =>
    fixture.smallGrid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity)
    fixture.smallGrid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity)

    for {
      atMaxDistancePath <- fixture.ops.generateRoamPath(
        fixture.smallGrid,
        (0, 1),
        Distance(4),
        TraversalMode.OnLand
      )
      belowMaxDistancePath1 <- fixture.ops.generateRoamPath(
        fixture.smallGrid,
        (0, 1),
        Distance(10),
        TraversalMode.OnLand
      )
      belowMaxDistancePath2 <- fixture.ops.generateRoamPath(
        fixture.smallGrid,
        (0, 1),
        Distance(42),
        TraversalMode.OnLand
      )
    } yield {
      atMaxDistancePath should be(Queue[Point]((0, 0), (1, 0), (2, 0)))
      belowMaxDistancePath1 should be(Queue[Point]((0, 0), (1, 0), (2, 0), (2, 1), (2, 2)))
      belowMaxDistancePath2 should be(belowMaxDistancePath1)
    }
  }

  they should "calculate passable neighbours" in { fixture =>
    fixture.smallGrid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity)
    fixture.smallGrid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity)
    fixture.smallGrid.getUnsafe((1, 0)) ! AddEntity(existingRoadblockMapEntity)
    fixture.smallGrid.getUnsafe((0, 1)) ! AddEntity(existingRoadMapEntity)
    fixture.smallGrid.getUnsafe((0, 2)) ! AddEntity(existingRoadMapEntity)
    fixture.smallGrid.getUnsafe((2, 1)) ! AddEntity(existingRoadMapEntity)

    for {
      neighboursOf00 <- fixture.ops.passableNeighboursOf(
        fixture.smallGrid,
        (0, 0),
        TraversalMode.OnLand,
        roadblockRestricted = false
      )
      neighboursOf01 <- fixture.ops.passableNeighboursOf(
        fixture.smallGrid,
        (0, 1),
        TraversalMode.OnLand,
        roadblockRestricted = false
      )
      neighboursOf22 <- fixture.ops.passableNeighboursOf(
        fixture.smallGrid,
        (2, 2),
        TraversalMode.OnLand,
        roadblockRestricted = false
      )
      neighboursOf00WithRoadblockRestriction <- fixture.ops.passableNeighboursOf(
        fixture.smallGrid,
        (0, 0),
        TraversalMode.RoadRequired,
        roadblockRestricted = true
      )
      neighboursOf01WithRoadblockRestriction <- fixture.ops.passableNeighboursOf(
        fixture.smallGrid,
        (0, 1),
        TraversalMode.RoadRequired,
        roadblockRestricted = true
      )
      neighboursOf22WithRoadblockRestriction <- fixture.ops.passableNeighboursOf(
        fixture.smallGrid,
        (2, 2),
        TraversalMode.RoadRequired,
        roadblockRestricted = true
      )
    } yield {
      neighboursOf00 should contain.theSameElementsAs(Seq[Point]((1, 0), (0, 1)))
      neighboursOf01 should contain.theSameElementsAs(Seq[Point]((0, 0), (1, 0), (0, 2)))
      neighboursOf22 should contain.theSameElementsAs(Seq[Point]((2, 1)))
      neighboursOf00WithRoadblockRestriction should contain.theSameElementsAs(Seq[Point]((0, 1)))
      neighboursOf01WithRoadblockRestriction should contain.theSameElementsAs(Seq[Point]((0, 2)))
      neighboursOf22WithRoadblockRestriction should contain.theSameElementsAs(Seq[Point]((2, 1)))
    }
  }

  they should "calculate diagonal movement restrictions" in { fixture =>
    fixture.ops.diagonalMovementAllowed(TraversalMode.RoadRequired) should be(false)
    fixture.ops.diagonalMovementAllowed(TraversalMode.RoadPreferred) should be(false)
    fixture.ops.diagonalMovementAllowed(TraversalMode.OnLand) should be(true)
    fixture.ops.diagonalMovementAllowed(TraversalMode.OnWater) should be(true)
  }

  they should "generate advance paths that follow roads" in { fixture =>
    for {
      invalidPathToBeach <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (6, 2),
        (7, 1),
        TraversalMode.RoadRequired
      )
      invalidPathToWater <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (6, 2),
        (7, 5),
        TraversalMode.RoadRequired
      )
      invalidPathWithoutRoad <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (6, 2),
        (6, 5),
        TraversalMode.RoadRequired
      )
      validPathToLand <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (6, 2),
        (0, 8),
        TraversalMode.RoadRequired
      )
    } yield {
      invalidPathToBeach should be(Queue.empty[Point])
      invalidPathToWater should be(Queue.empty[Point])
      invalidPathWithoutRoad should be(Queue.empty[Point])
      validPathToLand should be(
        Queue[Point](
          (6, 1),
          (5, 1),
          (4, 1),
          (3, 1),
          (3, 2),
          (3, 3),
          (3, 4),
          (3, 5),
          (3, 6),
          (2, 6),
          (1, 6),
          (1, 7),
          (0, 7),
          (0, 8)
        )
      )
    }
  }

  they should "generate advance paths that prefer roads" in { fixture =>
    for {
      roadPath <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        start = (6, 2),
        end = (1, 6),
        traversalMode = TraversalMode.RoadPreferred
      )
      landPath <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        start = (6, 2),
        end = (0, 6),
        traversalMode = TraversalMode.RoadPreferred
      )
    } yield {
      roadPath should be(
        Queue[Point](
          (6, 1),
          (5, 1),
          (4, 1),
          (3, 1),
          (3, 2),
          (3, 3),
          (3, 4),
          (3, 5),
          (3, 6),
          (2, 6),
          (1, 6)
        )
      )
      landPath should be(
        Queue[Point](
          (6, 3),
          (5, 4),
          (4, 5),
          (3, 6),
          (2, 6),
          (1, 6),
          (0, 6)
        )
      )
    }
  }

  they should "generate land-based advance paths" in { fixture =>
    for {
      invalidPathToBeach <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (6, 2),
        (7, 1),
        TraversalMode.OnLand
      )
      invalidPathToWater <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (6, 2),
        (7, 5),
        TraversalMode.OnLand
      )
      validPathToLand <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (6, 2),
        (0, 8),
        TraversalMode.OnLand
      )
      validPathToFloodplain <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (6, 2),
        (5, 8),
        TraversalMode.OnLand
      )
    } yield {
      invalidPathToBeach should be(Queue.empty[Point])
      invalidPathToWater should be(Queue.empty[Point])
      validPathToLand should be(Queue[Point]((6, 3), (5, 4), (4, 5), (3, 6), (2, 6), (1, 7), (0, 8)))
      validPathToFloodplain should be(Queue[Point]((6, 3), (5, 4), (5, 5), (5, 6), (5, 7), (5, 8)))
    }
  }

  they should "generate water-based advance paths" in { fixture =>
    for {
      invalidPathToLand <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (8, 5),
        (6, 5),
        TraversalMode.OnWater
      )
      invalidPathToBeach <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (8, 5),
        (8, 4),
        TraversalMode.OnWater
      )
      invalidPathToFloodplain <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (8, 5),
        (6, 6),
        TraversalMode.OnWater
      )
      validPath <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (8, 5),
        (6, 8),
        TraversalMode.OnWater
      )
    } yield {
      invalidPathToLand should be(Queue.empty[Point])
      invalidPathToBeach should be(Queue.empty[Point])
      invalidPathToFloodplain should be(Queue.empty[Point])
      validPath should be(Queue[Point]((7, 6), (7, 7), (6, 8)))
    }
  }

  they should "not generate beach-based advance paths" in { fixture =>
    for {
      invalidPathToLand <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (8, 0),
        (5, 0),
        TraversalMode.OnLand
      )
      invalidPathToBeach <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (8, 0),
        (8, 4),
        TraversalMode.OnLand
      )
      invalidPathToFloodplain <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (8, 0),
        (6, 6),
        TraversalMode.OnLand
      )
      invalidPathToWater <- fixture.ops.generateAdvancePath(
        fixture.largeGrid,
        (8, 0),
        (8, 5),
        TraversalMode.OnLand
      )
    } yield {
      invalidPathToLand should be(Queue.empty[Point])
      invalidPathToBeach should be(Queue.empty[Point])
      invalidPathToFloodplain should be(Queue.empty[Point])
      invalidPathToWater should be(Queue.empty[Point])
    }
  }

  they should "generate roadblock-restricted roam paths" in { fixture =>
    for {
      longPath <- fixture.ops.generateRoamPath(
        grid = fixture.largeGrid,
        start = (6, 2),
        maxDistance = Distance(16),
        traversalMode = TraversalMode.RoadRequired
      )
      shortPath <- fixture.ops.generateRoamPath(
        grid = fixture.largeGrid,
        start = (3, 5),
        maxDistance = Distance(16),
        traversalMode = TraversalMode.RoadRequired
      )
    } yield {
      longPath should be(
        Queue[Point](
          (6, 1),
          (5, 1),
          (4, 1),
          (3, 1),
          (3, 2),
          (3, 3),
          (2, 3),
          (1, 3),
          (0, 3),
          (0, 4),
          (0, 5),
          (1, 5),
          (1, 6),
          (1, 7),
          (0, 7)
        )
      )

      shortPath should be(Queue[Point]((3, 6)))
    }
  }

  private val existingStructureEntity = new Structure {
    override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
    override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, effects.Effect)] = ???
    override protected def createBehaviour(): BaseStructure = ???
    override def `size`: Entity.Size = Entity.Size(2, 1)
    override def `desirability`: Desirability = Desirability.Min
  }

  private val existingStructureMapEntity = MapEntity(
    entityRef = StructureRef(TestProbe().ref),
    parentCell = Point(1, 1),
    spec = existingStructureEntity
  )

  private val existingRoadblockMapEntity = MapEntity(
    entityRef = RoadblockRef(TestProbe().ref),
    parentCell = Point(1, 0),
    spec = new Roadblock
  )

  private val existingRoadMapEntity = MapEntity(
    entityRef = RoadRef(TestProbe().ref),
    parentCell = Point(1, 0),
    spec = new Road
  )

  private val largeStructureMapEntity = MapEntity(
    entityRef = StructureRef(TestProbe().ref),
    parentCell = Point(0, 0),
    spec = new Structure {
      override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
      override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, effects.Effect)] = ???
      override protected def createBehaviour(): BaseStructure = ???
      override def `size`: Entity.Size = Entity.Size(3, 3)
      override def `desirability`: Desirability = Desirability.Min
    }
  )

  private val resourceMapEntity01 = MapEntity(
    entityRef = StructureRef(TestProbe().ref),
    parentCell = Point(1, 4),
    spec = new Resource {
      override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
      override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, effects.Effect)] = ???
      override protected def createBehaviour(): BaseResource = ???
    }
  )

  private val resourceMapEntity02 = MapEntity(
    entityRef = StructureRef(TestProbe().ref),
    parentCell = Point(2, 5),
    spec = new Resource {
      override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
      override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, effects.Effect)] = ???
      override protected def createBehaviour(): BaseResource = ???
    }
  )

  private val baseSmallGridUpdates: Map[Point, Seq[Cell.Message]] = Map(
    Point(0, 0) -> Seq(),
    Point(1, 0) -> Seq(),
    Point(2, 0) -> Seq(),
    Point(0, 1) -> Seq(),
    Point(1, 1) -> Seq(),
    Point(2, 1) -> Seq(),
    Point(0, 2) -> Seq(),
    Point(1, 2) -> Seq(),
    Point(2, 2) -> Seq()
  )

  private val baseLargeGridUpdates: Map[Point, Seq[Cell.Message]] = Map(
    Point(0, 0) -> Seq(AddEntity(largeStructureMapEntity)),
    Point(1, 0) -> Seq(AddEntity(largeStructureMapEntity)),
    Point(2, 0) -> Seq(AddEntity(largeStructureMapEntity)),
    Point(3, 0) -> Seq(),
    Point(4, 0) -> Seq(),
    Point(5, 0) -> Seq(),
    Point(6, 0) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(7, 0) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(8, 0) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(0, 1) -> Seq(AddEntity(largeStructureMapEntity)),
    Point(1, 1) -> Seq(AddEntity(largeStructureMapEntity)),
    Point(2, 1) -> Seq(AddEntity(largeStructureMapEntity)),
    Point(3, 1) -> Seq(AddEntity(newRoadMapEntity((3, 1)))),
    Point(4, 1) -> Seq(AddEntity(newRoadMapEntity((4, 1)))),
    Point(5, 1) -> Seq(AddEntity(newRoadMapEntity((5, 1)))),
    Point(6, 1) -> Seq(AddEntity(newRoadMapEntity((6, 1)))),
    Point(7, 1) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(8, 1) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(0, 2) -> Seq(AddEntity(largeStructureMapEntity)),
    Point(1, 2) -> Seq(AddEntity(largeStructureMapEntity)),
    Point(2, 2) -> Seq(AddEntity(largeStructureMapEntity)),
    Point(3, 2) -> Seq(AddEntity(newRoadMapEntity((3, 2)))),
    Point(4, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(5, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(6, 2) -> Seq(AddEntity(newRoadMapEntity((6, 2)))),
    Point(7, 2) -> Seq(),
    Point(8, 2) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(0, 3) -> Seq(AddEntity(newRoadMapEntity((0, 3)))),
    Point(1, 3) -> Seq(AddEntity(newRoadMapEntity((1, 3)))),
    Point(2, 3) -> Seq(AddEntity(newRoadMapEntity((2, 3)))),
    Point(3, 3) -> Seq(AddEntity(newRoadMapEntity((3, 3)))),
    Point(4, 3) -> Seq(UpdateType(Cell.Type.Water)),
    Point(5, 3) -> Seq(UpdateType(Cell.Type.Water)),
    Point(6, 3) -> Seq(),
    Point(7, 3) -> Seq(),
    Point(8, 3) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(0, 4) -> Seq(AddEntity(newRoadMapEntity((0, 4)))),
    Point(1, 4) -> Seq(AddEntity(resourceMapEntity01)),
    Point(2, 4) -> Seq(),
    Point(3, 4) -> Seq(AddEntity(newRoadMapEntity((3, 4))), AddEntity(newRoadblockMapEntity((3, 4)))),
    Point(4, 4) -> Seq(),
    Point(5, 4) -> Seq(),
    Point(6, 4) -> Seq(),
    Point(7, 4) -> Seq(),
    Point(8, 4) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(0, 5) -> Seq(AddEntity(newRoadMapEntity((0, 5)))),
    Point(1, 5) -> Seq(AddEntity(newRoadMapEntity((1, 5)))),
    Point(2, 5) -> Seq(AddEntity(resourceMapEntity02)),
    Point(3, 5) -> Seq(AddEntity(newRoadMapEntity((3, 5)))),
    Point(4, 5) -> Seq(),
    Point(5, 5) -> Seq(),
    Point(6, 5) -> Seq(),
    Point(7, 5) -> Seq(UpdateType(Cell.Type.Water)),
    Point(8, 5) -> Seq(UpdateType(Cell.Type.Water)),
    Point(0, 6) -> Seq(),
    Point(1, 6) -> Seq(AddEntity(newRoadMapEntity((1, 6)))),
    Point(2, 6) -> Seq(AddEntity(newRoadMapEntity((2, 6))), AddEntity(newRoadblockMapEntity((2, 6)))),
    Point(3, 6) -> Seq(AddEntity(newRoadMapEntity((3, 6)))),
    Point(4, 6) -> Seq(),
    Point(5, 6) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(6, 6) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(7, 6) -> Seq(UpdateType(Cell.Type.Water)),
    Point(8, 6) -> Seq(UpdateType(Cell.Type.Water)),
    Point(0, 7) -> Seq(AddEntity(newRoadMapEntity((0, 7)))),
    Point(1, 7) -> Seq(AddEntity(newRoadMapEntity((1, 7)))),
    Point(2, 7) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(3, 7) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(4, 7) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(5, 7) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(6, 7) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(7, 7) -> Seq(UpdateType(Cell.Type.Water)),
    Point(8, 7) -> Seq(UpdateType(Cell.Type.Water)),
    Point(0, 8) -> Seq(AddEntity(newRoadMapEntity((0, 8)))),
    Point(1, 8) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(2, 8) -> Seq(UpdateType(Cell.Type.Beach)),
    Point(3, 8) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(4, 8) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(5, 8) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(6, 8) -> Seq(UpdateType(Cell.Type.Water)),
    Point(7, 8) -> Seq(UpdateType(Cell.Type.Water)),
    Point(8, 8) -> Seq(UpdateType(Cell.Type.Water))
  )

  private def newRoadMapEntity(point: Point): MapEntity = MapEntity(
    entityRef = RoadRef(TestProbe().ref),
    parentCell = point,
    spec = new Road
  )

  private def newRoadblockMapEntity(point: Point): MapEntity = MapEntity(
    entityRef = RoadblockRef(TestProbe().ref),
    parentCell = point,
    spec = new Roadblock
  )

  private def initGrid(grid: Grid[CellActorRef], gridUpdates: Map[Point, Seq[Cell.Message]]): Unit =
    grid.foreachIndexed {
      case (point, ref) =>
        gridUpdates.getOrElse(point, Seq.empty).foreach(m => ref ! m)
    }
}
