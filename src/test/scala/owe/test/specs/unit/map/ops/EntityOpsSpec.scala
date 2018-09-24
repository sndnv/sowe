package owe.test.specs.unit.map.ops

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.Tagging._
import owe.effects
import owe.entities.{ActiveEntity, Entity}
import owe.entities.Entity.{Desirability, EntityRef}
import owe.entities.active.{Structure, Walker}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.{SpawnLocation, WalkerRef}
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.passive.Road
import owe.entities.passive.Road.RoadRef
import owe.events.Event
import owe.map.Cell._
import owe.map.grid.{Grid, Point}
import owe.map.ops.{AvailabilityOps, EntityOps}
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AsyncUnitSpec

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import owe.events.Event.{CellEvent, EntityEvent, SystemEvent}
import owe.test.specs.unit.entities.definitions.active.walkers.Archer

class EntityOpsSpec extends AsyncUnitSpec {
  private implicit val timeout: Timeout = 3.seconds

  private class Ops extends EntityOps with AvailabilityOps {
    override protected implicit val actionTimeout: Timeout = timeout
    override protected implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  }

  private implicit val system: ActorSystem = ActorSystem()

  private def existingStructureEntity(withSize: Entity.Size): Structure = new Structure {
    override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
    override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, effects.Effect)] = ???
    override protected def createBehaviour(): BaseStructure = ???
    override def `size`: Entity.Size = withSize
    override def `desirability`: Desirability = Desirability.Min
  }

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
      spec = existingStructureEntity(withSize = Entity.Size(1, 1))
    )

    val unavailableEntityCell = Point(1, 0)
    val newStructure = new Structure {
      override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
      override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, effects.Effect)] = ???
      override protected def createBehaviour(): BaseStructure = ???
      override def `size`: Entity.Size = Entity.Size(2, 2)
      override def `desirability`: Desirability = Desirability.Min
    }

    val roadCell = Point(0, 1)
    val roadEntity = new Road()

    val walkerCell = Point(2, 2)
    trait TestWalker extends Walker {
      override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
      override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, effects.Effect)] = ???
      override protected def createBehaviour(): BaseWalker = ???
    }

    fixture.grid.getUnsafe(unavailableMainCell) ! AddEntity(existingStructureMapEntity)

    for {
      outOfBoundsCellResult <- fixture.ops.createEntity(
        fixture.grid,
        Map.empty,
        roadEntity,
        TestProbe().ref,
        outOfBoundsCell
      )
      unavailableMainCellResult <- fixture.ops.createEntity(
        fixture.grid,
        Map.empty,
        newStructure,
        TestProbe().ref,
        unavailableMainCell
      )
      unavailableEntityCellsResult <- fixture.ops.createEntity(
        fixture.grid,
        Map.empty,
        newStructure,
        TestProbe().ref,
        unavailableEntityCell
      )
      adjacentSpawnPointUnavailableResult <- fixture.ops.createEntity(
        fixture.grid,
        Map.empty,
        new TestWalker {
          override def spawnLocation: SpawnLocation = SpawnLocation.AdjacentPoint(StructureRef(TestProbe().ref))
        },
        TestProbe().ref,
        walkerCell
      )
      adjacentRoadUnavailableResult <- fixture.ops.createEntity(
        fixture.grid,
        Map.empty,
        new TestWalker {
          override def spawnLocation: SpawnLocation = SpawnLocation.AdjacentRoad(StructureRef(TestProbe().ref))
        },
        TestProbe().ref,
        walkerCell
      )
      successfulResult <- fixture.ops.createEntity(
        fixture.grid,
        Map.empty,
        roadEntity,
        TestProbe().ref,
        roadCell
      )
    } yield {
      outOfBoundsCellResult should be(Left(CellEvent(Event.Engine.CellOutOfBounds, outOfBoundsCell)))
      unavailableMainCellResult should be(Left(CellEvent(Event.Engine.CellsUnavailable, unavailableMainCell)))
      unavailableEntityCellsResult should be(Left(CellEvent(Event.Engine.CellsUnavailable, unavailableEntityCell)))
      adjacentSpawnPointUnavailableResult should be(Left(SystemEvent(Event.Engine.SpawnPointUnavailable)))
      adjacentRoadUnavailableResult should be(Left(SystemEvent(Event.Engine.SpawnPointUnavailable)))

      successfulResult match {
        case Left(event)               => fail(s"Expected successful result but event [$event] encountered")
        case Right((mapEntity, event)) => event should be(EntityEvent(Event.Engine.EntityCreated, mapEntity, roadCell))
      }
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
      spec = new Archer
    )

    val existingStructureCell = expectedEntityCell
    val existingStructureEntityID = StructureRef(TestProbe().ref)
    val existingStructureMapEntity = MapEntity(
      entityRef = existingStructureEntityID,
      parentCell = existingStructureCell,
      spec = existingStructureEntity(withSize = Entity.Size(2, 2))
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
      outOfBoundsCellResult should be(Left(SystemEvent(Event.Engine.CellOutOfBounds)))
      entityMissingResult should be(Left(CellEvent(Event.Engine.EntityMissing, missingEntityCell)))
      expectedEntityNotFound should be(Left(CellEvent(Event.Engine.EntityMissing, expectedEntityCell)))

      successfulResult should be(
        Right((EntityEvent(Event.Engine.EntityDestroyed, mapEntity, entityCell), mapEntity, entityCell)))
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
      spec = existingStructureEntity(withSize = Entity.Size(2, 2))
    )

    val walkerCell = Point(0, 0)
    val multicellWalkerEntityID = WalkerRef(TestProbe().ref)
    val multicellWalkerMapEntity = MapEntity(
      entityRef = multicellWalkerEntityID,
      parentCell = walkerCell,
      spec = existingStructureEntity(withSize = Entity.Size(2, 2))
    )

    val newCell = Point(0, 1)
    val walkerEntityID = WalkerRef(TestProbe().ref)
    val walkerMapEntity = MapEntity(
      entityRef = walkerEntityID,
      parentCell = walkerCell,
      spec = new Archer
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
      currentCellNotFoundResult should be(Left(SystemEvent(Event.Engine.CellsUnavailable)))
      currentMapCellNotFoundResult should be(Left(CellEvent(Event.Engine.CellOutOfBounds, outOfBoundsCell)))
      newCellNotFoundResult should be(Left(CellEvent(Event.Engine.CellOutOfBounds, outOfBoundsCell)))
      currentEntityNotFoundResult should be(Left(CellEvent(Event.Engine.CellsUnavailable, newCell)))
      newCellUnavailableResult should be(Left(CellEvent(Event.Engine.CellsUnavailable, unavailableNewCell)))
      someEntityCellUnavailableResult should be(Left(CellEvent(Event.Engine.CellsUnavailable, newCell)))

      successfulResult should be(
        Right((EntityEvent(Event.Engine.EntityMoved, walkerMapEntity, newCell), walkerMapEntity, walkerCell)))
    }
  }

  they should "associate map entities" in { fixture =>
    val entities = Map.empty[EntityRef, Point]

    val roadEntityID = RoadRef(TestProbe().ref)
    val roadCell = Point(0, 0)
    val roadMapEntity = MapEntity(
      entityRef = roadEntityID,
      parentCell = roadCell,
      spec = new Road
    )

    val walkerEntityID = WalkerRef(TestProbe().ref)
    val walkerMapEntity = MapEntity(
      entityRef = walkerEntityID,
      parentCell = Point(0, 1),
      spec = new Archer
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
      spec = new Road
    )
    val roadCell = Point(0, 0)

    val walkerMapEntity = MapEntity(
      entityRef = walkerEntityID,
      parentCell = Point(0, 1),
      spec = new Archer
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
