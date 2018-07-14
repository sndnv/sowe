package owe.test.specs.unit.map.ops

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.Tagging._
import owe.entities.Entity
import owe.entities.Entity.{Desirability, EntityRef}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Road.RoadRef
import owe.events.Event
import owe.map.Cell._
import owe.map.grid.{Grid, Point}
import owe.map.ops.{AvailabilityOps, EntityOps}
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AsyncUnitSpec

class EntityOpsSpec extends AsyncUnitSpec {
  private implicit val timeout: Timeout = 3.seconds

  private class Ops extends EntityOps with AvailabilityOps {
    override protected implicit val actionTimeout: Timeout = timeout
    override protected implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  }

  private implicit val system: ActorSystem = ActorSystem()

  case class FixtureParam(ops: EntityOps, grid: Grid[CellActorRef])

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

  "Entity ops" should "create entities" in { fixture =>
    val outOfBoundsCell = Point(7, 12)

    val unavailableMainCell = Point(1, 1)
    val existingStructureEntityID = StructureRef(TestProbe().ref)
    val existingStructureMapEntity = MapEntity(
      entityRef = existingStructureEntityID,
      parentCell = unavailableMainCell,
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )

    val unavailableEntityCell = Point(1, 0)
    val newStructureEntityID = StructureRef(TestProbe().ref)
    val newStructureMapEntity = MapEntity(
      entityRef = newStructureEntityID,
      parentCell = unavailableMainCell,
      size = Entity.Size(2, 2),
      desirability = Desirability.Min
    )

    val roadCell = Point(0, 1)
    val roadEntityID = RoadRef(TestProbe().ref)
    val roadMapEntity = MapEntity(
      entityRef = roadEntityID,
      parentCell = roadCell,
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )

    val entities = Map[EntityRef, Point](existingStructureEntityID -> unavailableMainCell)
    fixture.grid.getUnsafe(unavailableMainCell) ! AddEntity(existingStructureMapEntity)

    for {
      outOfBoundsCellResult <- fixture.ops.createEntity(
        fixture.grid,
        entities,
        roadMapEntity,
        outOfBoundsCell
      )
      unavailableMainCellResult <- fixture.ops.createEntity(
        fixture.grid,
        entities,
        newStructureMapEntity,
        unavailableMainCell
      )
      unavailableEntityCellsResult <- fixture.ops.createEntity(
        fixture.grid,
        entities,
        newStructureMapEntity,
        unavailableEntityCell
      )
      successfulResult <- fixture.ops.createEntity(fixture.grid, entities, roadMapEntity, roadCell)
    } yield {
      outOfBoundsCellResult should be((entities, Event(Event.System.CellOutOfBounds, Some(outOfBoundsCell))))
      unavailableMainCellResult should be((entities, Event(Event.System.CellsUnavailable, Some(unavailableMainCell))))
      unavailableEntityCellsResult should be(
        (entities, Event(Event.System.CellsUnavailable, Some(unavailableEntityCell))))

      val updatedEntities = entities + (roadEntityID -> roadCell)
      successfulResult should be((updatedEntities, Event(Event.System.EntityCreated, Some(roadCell))))
    }
  }

  they should "destroy entities" in { fixture =>
    val outOfBoundsCell = Point(7, 12)
    val outOfBoundsEntityID = WalkerRef(TestProbe().ref)

    val missingEntityCell = Point(1, 1)
    val missingEntityID = WalkerRef(TestProbe().ref)

    val expectedEntityCell = Point(1, 0)
    val expectedEntityID = WalkerRef(TestProbe().ref)

    val entityCell = Point(2, 2)
    val entityID = WalkerRef(TestProbe().ref)
    val mapEntity = MapEntity(
      entityRef = entityID,
      parentCell = entityCell,
      size = Entity.Size(2, 2),
      desirability = Desirability.Min
    )

    val existingStructureCell = expectedEntityCell
    val existingStructureEntityID = StructureRef(TestProbe().ref)
    val existingStructureMapEntity = MapEntity(
      entityRef = existingStructureEntityID,
      parentCell = existingStructureCell,
      size = Entity.Size(2, 2),
      desirability = Desirability.Min
    )

    val entities = Map[EntityRef, Point](
      outOfBoundsEntityID -> outOfBoundsCell,
      missingEntityID -> missingEntityCell,
      expectedEntityID -> expectedEntityCell,
      existingStructureEntityID -> existingStructureCell,
      entityID -> entityCell
    )
    fixture.grid.getUnsafe(existingStructureCell) ! AddEntity(existingStructureMapEntity)
    fixture.grid.getUnsafe(entityCell) ! AddEntity(mapEntity)

    for {
      outOfBoundsCellResult <- fixture.ops.destroyEntity(fixture.grid, entities, outOfBoundsEntityID)
      entityMissingResult <- fixture.ops.destroyEntity(fixture.grid, entities, missingEntityID)
      expectedEntityNotFound <- fixture.ops.destroyEntity(fixture.grid, entities, expectedEntityID)
      successfulResult <- fixture.ops.destroyEntity(fixture.grid, entities, entityID)
    } yield {
      outOfBoundsCellResult should be((entities, Event(Event.System.CellOutOfBounds, cell = None)))
      entityMissingResult should be((entities, Event(Event.System.EntityMissing, Some(missingEntityCell))))
      expectedEntityNotFound should be((entities, Event(Event.System.EntityMissing, Some(expectedEntityCell))))

      val updatedEntities = entities - entityID
      successfulResult should be((updatedEntities, Event(Event.System.EntityDestroyed, Some(entityCell))))
    }
  }

  they should "move entities" in { fixture =>
    val missingEntityID = WalkerRef(TestProbe().ref)

    val outOfBoundsCell = Point(7, 12)
    val outOfBoundsEntityID = WalkerRef(TestProbe().ref)

    val missingMapEntityCell = Point(2, 2)
    val missingMapEntityID = WalkerRef(TestProbe().ref)

    val unavailableNewCell = Point(1, 1)

    val existingStructureCell = unavailableNewCell
    val existingStructureEntityID = StructureRef(TestProbe().ref)
    val existingStructureMapEntity = MapEntity(
      entityRef = existingStructureEntityID,
      parentCell = unavailableNewCell,
      size = Entity.Size(2, 2),
      desirability = Desirability.Min
    )

    val walkerCell = Point(0, 0)
    val multicellWalkerEntityID = WalkerRef(TestProbe().ref)
    val multicellWalkerMapEntity = MapEntity(
      entityRef = multicellWalkerEntityID,
      parentCell = walkerCell,
      size = Entity.Size(2, 2),
      desirability = Desirability.Min
    )

    val newCell = Point(0, 1)
    val walkerEntityID = WalkerRef(TestProbe().ref)
    val walkerMapEntity = MapEntity(
      entityRef = walkerEntityID,
      parentCell = walkerCell,
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )

    val entities = Map[EntityRef, Point](
      outOfBoundsEntityID -> outOfBoundsCell,
      missingMapEntityID -> missingMapEntityCell,
      existingStructureEntityID -> existingStructureCell,
      multicellWalkerEntityID -> walkerCell,
      walkerEntityID -> walkerCell
    )
    fixture.grid.getUnsafe(existingStructureCell) ! AddEntity(existingStructureMapEntity)
    fixture.grid.getUnsafe(walkerCell) ! AddEntity(multicellWalkerMapEntity)
    fixture.grid.getUnsafe(walkerCell) ! AddEntity(walkerMapEntity)

    for {
      currentCellNotFoundResult <- fixture.ops.moveEntity(
        fixture.grid,
        entities,
        missingEntityID,
        newCell
      )
      currentMapCellNotFoundResult <- fixture.ops.moveEntity(
        fixture.grid,
        entities,
        outOfBoundsEntityID,
        newCell
      )
      newCellNotFoundResult <- fixture.ops.moveEntity(
        fixture.grid,
        entities,
        walkerEntityID,
        outOfBoundsCell
      )
      currentEntityNotFoundResult <- fixture.ops.moveEntity(
        fixture.grid,
        entities,
        missingMapEntityID,
        newCell
      )
      newCellUnavailableResult <- fixture.ops.moveEntity(
        fixture.grid,
        entities,
        walkerEntityID,
        unavailableNewCell
      )
      someEntityCellUnavailableResult <- fixture.ops.moveEntity(
        fixture.grid,
        entities,
        multicellWalkerEntityID,
        newCell
      )
      successfulResult <- fixture.ops.moveEntity(
        fixture.grid,
        entities,
        walkerEntityID,
        newCell
      )
    } yield {
      currentCellNotFoundResult should be((entities, Event(Event.System.CellsUnavailable, cell = None)))
      currentMapCellNotFoundResult should be((entities, Event(Event.System.CellOutOfBounds, Some(outOfBoundsCell))))
      newCellNotFoundResult should be((entities, Event(Event.System.CellOutOfBounds, Some(outOfBoundsCell))))
      currentEntityNotFoundResult should be((entities, Event(Event.System.CellsUnavailable, Some(newCell))))
      newCellUnavailableResult should be((entities, Event(Event.System.CellsUnavailable, Some(unavailableNewCell))))
      someEntityCellUnavailableResult should be((entities, Event(Event.System.CellsUnavailable, Some(newCell))))

      val updatedEntities = entities + (walkerEntityID -> newCell)
      successfulResult should be((updatedEntities, Event(Event.System.EntityMoved, Some(newCell))))
    }
  }

  they should "associate map entities" in { fixture =>
    val entities = Map.empty[EntityRef, Point]

    val roadEntityID = RoadRef(TestProbe().ref)
    val roadCell = Point(0, 0)
    val roadMapEntity = MapEntity(
      entityRef = roadEntityID,
      parentCell = roadCell,
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )

    val walkerEntityID = WalkerRef(TestProbe().ref)
    val walkerMapEntity = MapEntity(
      entityRef = walkerEntityID,
      parentCell = Point(0, 1),
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )
    val walkerCell = Point(0, 1)

    val entitiesWithRoad = fixture.ops.associateMapEntity(fixture.grid, entities, roadMapEntity, roadCell)
    entitiesWithRoad should be(Map(roadEntityID -> Point(0, 0)))

    val entitiesWithWalkerAndRoad =
      fixture.ops.associateMapEntity(fixture.grid, entitiesWithRoad, walkerMapEntity, walkerCell)
    entitiesWithWalkerAndRoad should be(
      Map(
        roadEntityID -> Point(0, 0),
        walkerEntityID -> Point(0, 1)
      )
    )
  }

  they should "dissociate map entities" in { fixture =>
    val roadEntityID = RoadRef(TestProbe().ref)
    val walkerEntityID = WalkerRef(TestProbe().ref)

    val entities = Map[EntityRef, Point](roadEntityID -> Point(0, 0), walkerEntityID -> Point(0, 1))

    val roadMapEntity = MapEntity(
      entityRef = roadEntityID,
      parentCell = Point(0, 0),
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )
    val roadCell = Point(0, 0)

    val walkerMapEntity = MapEntity(
      entityRef = walkerEntityID,
      parentCell = Point(0, 1),
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )
    val walkerCell = Point(0, 1)

    val entitiesWithRoad = fixture.ops.dissociateMapEntity(fixture.grid, entities, roadMapEntity, roadCell)
    entitiesWithRoad should be(Map(walkerEntityID -> Point(0, 1)))

    val entitiesWithWalkerAndRoad =
      fixture.ops.dissociateMapEntity(fixture.grid, entitiesWithRoad, walkerMapEntity, walkerCell)
    entitiesWithWalkerAndRoad should be(Map.empty)
  }

  they should "apply desirability" in { fixture =>
    val testProbe = TestProbe()

    for {
      _ <- Future.successful(
        fixture.ops.addDesirability(
          grid = fixture.grid,
          desirability = Entity.Desirability.Max,
          cells = Seq(Point(1, 1))
        )
      )
      _ <- Future.successful(fixture.grid.getUnsafe(Point(1, 1)).tell(GetCellData(), testProbe.ref))
      maxDesirabilityResult <- Future.successful(testProbe.receiveOne(timeout.duration).asInstanceOf[CellData])
      _ <- Future.successful(
        fixture.ops.removeDesirability(
          grid = fixture.grid,
          desirability = Entity.Desirability.Max,
          cells = Seq(Point(1, 1))
        )
      )
      _ <- Future.successful(fixture.grid.getUnsafe(Point(1, 1)).tell(GetCellData(), testProbe.ref))
      minDesirabilityResult <- Future.successful(testProbe.receiveOne(timeout.duration).asInstanceOf[CellData])
    } yield {
      maxDesirabilityResult.state.desirability should be(Cell.Desirability.Max)
      minDesirabilityResult.state.desirability should be(Cell.Desirability.Min)
    }
  }
}
