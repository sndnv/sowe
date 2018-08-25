package owe.test.specs.unit.map.ops

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.Tagging._
import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor.GetData
import owe.entities.Entity.{Desirability, EntityRef}
import owe.entities.active.Structure
import owe.entities.active.Structure._
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.attributes.{Distance, Life}
import owe.entities.{ActiveEntity, Entity}
import owe.map.Cell.{AddEntity, CellActorRef, CellData}
import owe.map.grid.{Grid, Point}
import owe.map.ops.{AvailabilityOps, PathfindingOps, QueryOps}
import owe.map.pathfinding.{DepthFirstSearch, Search}
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AsyncUnitSpec
import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class QueryOpsSpec extends AsyncUnitSpec {
  private implicit val timeout: Timeout = 3.seconds

  private class TestEntity(data: ActiveEntity.Data) extends Actor {
    override def receive: Receive = {
      case GetData() => sender ! data
    }
  }

  private class Ops extends QueryOps with PathfindingOps with AvailabilityOps {
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

  private val existingStructureData = StructureData(
    properties = Structure.Properties(
      homePosition = Point(0, 0),
      name = "TestProducingStructure",
      walkers = NoWalkers,
      stages = SingleStage(
        stage = StageProperties(
          maxLife = Life(100),
          maxPeople = 15,
          minDesirability = Cell.Desirability.Neutral,
          commodityShortageLimit = 0
        )
      )
    ),
    state = Structure.State(
      risk = NoRisk,
      commodities = NoCommodities,
      housing = NoHousing,
      production = NoProduction,
      currentStage = DefaultStage,
      currentLife = Life(100),
      walkers = NoWalkers
    ),
    modifiers = Structure.StateModifiers(
      risk = NoRisk,
      commodities = NoCommodities,
      housing = NoHousing,
      production = NoProduction
    ),
    id = StructureRef(TestProbe().ref)
  )

  case class FixtureParam(ops: QueryOps, grid: Grid[CellActorRef])

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

  "Query ops" should "retrieve advance paths" in { fixture =>
    fixture.grid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity)
    fixture.grid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity)

    val invalidDestination = Point(13, 5)
    val destination = Point(2, 2)

    val missingEntityID = WalkerRef(TestProbe().ref)

    val walkerCell = Point(0, 1)
    val walkerEntityID = WalkerRef(TestProbe().ref)

    val entities = Map[EntityRef, Point](
      walkerEntityID -> walkerCell
    )

    for {
      missingEntityResult <- fixture.ops.getAdvancePath(fixture.grid, entities, missingEntityID, destination)
      invalidDestinationResult <- fixture.ops.getAdvancePath(fixture.grid, entities, walkerEntityID, invalidDestination)
      successfulResult <- fixture.ops.getAdvancePath(fixture.grid, entities, walkerEntityID, destination)
    } yield {
      missingEntityResult should be(Queue.empty[Point])
      invalidDestinationResult should be(Queue.empty[Point])
      successfulResult should be(Queue[Point]((1, 0), (2, 1), (2, 2)))
    }
  }

  they should "retrieve roaming paths" in { fixture =>
    fixture.grid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity)
    fixture.grid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity)

    val missingEntityID = WalkerRef(TestProbe().ref)

    val walkerCell = Point(0, 1)
    val walkerEntityID = WalkerRef(TestProbe().ref)

    val entities = Map[EntityRef, Point](
      walkerEntityID -> walkerCell
    )

    for {
      missingEntityResult <- fixture.ops.getRoamingPath(fixture.grid, entities, missingEntityID, Distance(5))
      successfulResult <- fixture.ops.getRoamingPath(fixture.grid, entities, walkerEntityID, Distance(5))
    } yield {
      missingEntityResult should be(Queue.empty[Point])
      successfulResult should be(Queue[Point]((0, 1), (0, 0), (1, 0), (2, 0), (2, 1)))
    }
  }

  they should "retrieve neighbours" in { fixture =>
    val structureRef = StructureRef(system.actorOf(Props(new TestEntity(existingStructureData))))
    fixture.grid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))
    fixture.grid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))

    val missingEntityID = WalkerRef(TestProbe().ref)

    val walkerCell = Point(0, 1)
    val walkerEntityID = WalkerRef(TestProbe().ref)

    val entities = Map[EntityRef, Point](
      walkerEntityID -> walkerCell
    )

    for {
      missingEntityResult <- fixture.ops.getNeighbours(fixture.grid, entities, missingEntityID, Distance(1))
      successfulResult <- fixture.ops.getNeighbours(fixture.grid, entities, walkerEntityID, Distance(1))
    } yield {
      missingEntityResult should be(Seq.empty)
      successfulResult should be(Seq((structureRef, existingStructureData)))
    }
  }

  they should "retrieve entities data for a cell" in { fixture =>
    val structureRef = StructureRef(system.actorOf(Props(new TestEntity(existingStructureData))))
    fixture.grid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))
    fixture.grid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))

    for {
      missingCellResult <- fixture.ops.getEntities(fixture.grid, (13, 5))
      successfulResult <- fixture.ops.getEntities(fixture.grid, (1, 1))
    } yield {
      missingCellResult should be(Seq.empty)
      successfulResult should be(
        Seq((existingStructureMapEntity.copy(entityRef = structureRef), Some(existingStructureData)))
      )
    }
  }

  they should "retrieve specific entity data" in { fixture =>
    val structureRef = StructureRef(system.actorOf(Props(new TestEntity(existingStructureData))))
    fixture.grid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))
    fixture.grid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))

    val missingEntityID = WalkerRef(TestProbe().ref)

    val entities = Map[EntityRef, Point](
      structureRef -> (1, 1)
    )

    for {
      missingEntityResult <- fixture.ops
        .getEntity(fixture.grid, entities, missingEntityID)
        .map(data => Right(data))
        .recover { case e => Left(e) }
      successfulResult <- fixture.ops.getEntity(fixture.grid, entities, structureRef)
    } yield {
      missingEntityResult match {
        case Left(e)     => e shouldBe an[IllegalStateException]
        case Right(data) => fail(s"Expected a failure but received data: [$data]")
      }

      successfulResult should be(existingStructureData)
    }
  }

  they should "retrieve grid data" in { fixture =>
    val structureRef = StructureRef(system.actorOf(Props(new TestEntity(existingStructureData))))
    fixture.grid.getUnsafe((1, 1)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))
    fixture.grid.getUnsafe((1, 2)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))

    for {
      gridMap <- fixture.ops.getGridData(fixture.grid).map(_.toMap)
    } yield {
      gridMap should be(
        Map(
          Point(0, 0) -> CellData.empty,
          Point(1, 0) -> CellData.empty,
          Point(2, 0) -> CellData.empty,
          Point(0, 1) -> CellData.empty,
          Point(1, 1) -> CellData.empty.copy(
            entities = Map(
              structureRef -> MapEntity(structureRef, (1, 1), Entity.Size(2, 1), Entity.Desirability.Min)
            )
          ),
          Point(2, 1) -> CellData.empty,
          Point(0, 2) -> CellData.empty,
          Point(1, 2) -> CellData.empty.copy(
            entities = Map(
              structureRef -> MapEntity(structureRef, (1, 1), Entity.Size(2, 1), Entity.Desirability.Min)
            )
          ),
          Point(2, 2) -> CellData.empty
        )
      )
    }
  }
}
