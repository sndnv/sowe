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
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Road.RoadRef
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
    fail("Not Implemented", new NotImplementedError())
  }

  they should "destroy entities" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "move entities" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  they should "associate map entities" in { fixture =>
    val entities = Map.empty[EntityRef, Point]

    val roadEntityID = RoadRef(TestProbe().ref)
    val roadMapEntity = MapEntity(
      entityRef = roadEntityID,
      parentCell = Point(0, 0),
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )
    val roadCell = Point(0, 0)

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
