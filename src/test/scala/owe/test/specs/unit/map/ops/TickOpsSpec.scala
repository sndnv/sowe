package owe.test.specs.unit.map.ops

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.FutureOutcome
import owe.Tagging._
import owe.effects.Effect
import owe.entities.ActiveEntityActor.GetActiveEffects
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.active.Structure.StructureRef
import owe.entities.passive.Road.RoadRef
import owe.map.Cell.{AddEntity, CellActorRef}
import owe.map.GameMap.TickProcessed
import owe.map.grid.{Grid, Point}
import owe.map.ops.TickOps
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AsyncUnitSpec
import owe.test.specs.unit.effects.definitions.IncreaseMovementSpeed

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class TickOpsSpec extends AsyncUnitSpec {
  private implicit val timeout: Timeout = 3.seconds

  private class TestEntity(effects: Seq[Effect]) extends Actor {
    override def receive: Receive = {
      case GetActiveEffects() => sender ! effects
    }
  }

  private class Ops extends TickOps {
    override protected implicit val actionTimeout: Timeout = timeout
    override protected implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global
  }

  private implicit val system: ActorSystem = ActorSystem()

  private val existingStructureMapEntity = MapEntity(
    entityRef = StructureRef(TestProbe().ref),
    parentCell = Point(0, 0),
    size = Entity.Size(1, 1),
    desirability = Desirability.Min
  )

  case class FixtureParam(ops: TickOps, grid: Grid[CellActorRef])

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

  "Tick ops" should "gather active effects" in { fixture =>
    val effects = Seq(new IncreaseMovementSpeed)

    val roadMapEntity = MapEntity(
      entityRef = RoadRef(TestProbe().ref),
      parentCell = Point(0, 1),
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )

    val structureRef = StructureRef(system.actorOf(Props(new TestEntity(effects))))
    fixture.grid.getUnsafe((0, 0)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))
    fixture.grid.getUnsafe((0, 1)) ! AddEntity(roadMapEntity)

    for {
      result <- fixture.ops.gatherActiveEffects(fixture.grid)
    } yield {
      result should be(
        Map(
          Point(0, 0) -> effects,
          Point(0, 1) -> effects,
          Point(1, 0) -> effects,
          Point(1, 1) -> effects
        )
      )
    }
  }

  they should "process cells" in { fixture =>
    val effects = Seq(
      new IncreaseMovementSpeed,
      new Cell.Effect {
        override def apply(state: Cell.State): Cell.State = state
        override def radius: Int = 1
      }
    )

    val activeEffects = Map(
      Point(0, 0) -> effects,
      Point(0, 1) -> effects,
      Point(1, 0) -> effects,
      Point(1, 1) -> effects
    )

    for {
      missingCellResult <- fixture.ops
        .processCells(fixture.grid, activeEffects, (13, 5), (2, 2))
        .map(data => Right(data))
        .recover { case e => Left(e) }
      successfulResult1 <- fixture.ops.processCells(fixture.grid, activeEffects, (0, 0), (2, 2))
      successfulResult2 <- fixture.ops.processCells(fixture.grid, activeEffects, (2, 1), (2, 2))
    } yield {
      missingCellResult match {
        case Left(e)     => e shouldBe an[IllegalArgumentException]
        case Right(data) => fail(s"Expected a failure but received processed cells: [$data]")
      }

      successfulResult1 should be(9)
      successfulResult2 should be(4)
    }
  }

  they should "process ticks" in { fixture =>
    val effects = Seq(new IncreaseMovementSpeed)

    val roadMapEntity = MapEntity(
      entityRef = RoadRef(TestProbe().ref),
      parentCell = Point(0, 1),
      size = Entity.Size(1, 1),
      desirability = Desirability.Min
    )

    val structureRef = StructureRef(system.actorOf(Props(new TestEntity(effects))))
    fixture.grid.getUnsafe((0, 0)) ! AddEntity(existingStructureMapEntity.copy(entityRef = structureRef))
    fixture.grid.getUnsafe((0, 1)) ! AddEntity(roadMapEntity)

    for {
      successfulResult1 <- fixture.ops.processTick(fixture.grid, (0, 0), (2, 2)).map(_.processedCells)
      successfulResult2 <- fixture.ops.processTick(fixture.grid, (2, 1), (2, 2)).map(_.processedCells)
    } yield {
      successfulResult1 should be(9)
      successfulResult2 should be(4)
    }
  }
}
