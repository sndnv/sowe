package owe.test.specs.unit.entities.active.behaviour.walker.roaming

import akka.actor.Props
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, StructureData, WalkerData}
import owe.entities.Entity
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker._
import owe.entities.active.attributes.{Distance, Life, Speed}
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.structure.housing.HousingStructure
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.roaming.Recruiter
import owe.entities.active.{Structure, Walker}
import owe.events.Event
import owe.map.GameMap.CreateEntity
import owe.map.grid.Point
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures
import owe.test.specs.unit.entities.active.behaviour.walker.WalkerBehaviour
import owe.test.specs.unit.entities.definitions.active.structures.StorageBuilding
import owe.test.specs.unit.map.TestGameMap
import owe.test.specs.unit.map.TestGameMap.StartBehaviour

import scala.concurrent.duration._

class RecruiterSpec extends AkkaUnitSpec("RecruiterSpec") with WalkerBehaviour {
  private implicit val timeout: Timeout = 5.seconds

  private class TestRecruiter(
    properties: Properties,
    state: State,
    modifiers: StateModifiers
  ) extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint

    override protected def createBehaviour(): BaseWalker = new Recruiter {
      override protected def recruitmentRadius: Distance = Distance(3)
    }

    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: WalkerRef =>
        WalkerData(properties, state, modifiers, id)
    }

    override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
  }

  private class TestStructure() extends Structure {
    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: StructureRef =>
        StructureData(
          properties = Fixtures.Structure.Housing.properties,
          state = Fixtures.Structure.Housing.state.copy(
            commodities = Structure.CommoditiesState(
              available = Map.empty,
              limits = Map.empty
            ),
            housing = Structure.HousingState(
              occupants = 5,
              commodityShortage = 0,
              education = Map.empty,
              entertainment = Map.empty,
              religion = Map.empty,
              healthcare = Map.empty,
              civilService = Map.empty
            )
          ),
          modifiers = Fixtures.Structure.Housing.modifiers,
          id
        )
    }

    override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
    override protected def createBehaviour(): BaseStructure = new HousingStructure {}
    override def `size`: Entity.Size = Entity.Size(1, 1)
    override def `desirability`: Entity.Desirability = Entity.Desirability.Neutral
  }

  private val properties: Properties = Fixtures.Walker.properties.copy(
    homePosition = Point(0, 0),
    name = "Recruiter",
    movementSpeed = Speed(1),
    attack = NoAttack
  )

  private val state: State = Fixtures.Walker.state.copy(
    currentLife = Life(50),
    commodities = NoCommodities,
    mode = MovementMode.Roaming
  )

  private val modifiers: StateModifiers = Fixtures.Walker.modifiers.copy(
    attack = NoAttack
  )

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Recruiter walker" should "roam around until labour is found" in { _ =>
    val testProbe = TestProbe()
    val map = system.actorOf(
      Props(
        new TestGameMap(
          testProbe.ref,
          StartBehaviour.Idle,
          interval = 400.millis,
          expiration = 500.millis
        )
      )
    )

    val structure = new TestStructure

    val parentBuilding = new StorageBuilding(
      walkerGenerators = Map(
        "recruiter" -> { data =>
          if (data.state.production.asInstanceOf[Structure.ProductionState].labour == Structure.LabourState.Found) {
            None
          } else {
            Some(
              new TestRecruiter(
                properties.copy(
                  parent = Some(data.id)
                ),
                state,
                modifiers
              )
            )
          }

        }
      )
    ) {
      override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
    }

    map.tell(CreateEntity(structure, (1, 0)), testProbe.ref)
    testProbe.expectMsg(Event(Event.System.EntityCreated, Some((1, 0))))
    testProbe.expectMsgType[StructureRef]

    map.tell(CreateEntity(parentBuilding, (2, 0)), testProbe.ref)
    testProbe.expectMsg(Event(Event.System.EntityCreated, Some((2, 0))))
    testProbe.expectMsgType[StructureRef]

    val (ticks, acted, created) = testProbe
      .receiveWhile(timeout.duration) {
        case Event(Event.System.TickProcessed, None) => (1, 0, 0)
        case Event(Event.System.MessageForwarded, _) => (0, 1, 0)
        case Event(Event.System.EntityCreated, _)    => (0, 0, 1)
        case _                                       => (0, 0, 0)
      }
      .foldLeft((0, 0, 0)) {
        case ((totalTicks, totalActed, totalCreated), (currentTick, currentActed, currentCreated)) =>
          (
            totalTicks + currentTick,
            totalActed + currentActed,
            totalCreated + currentCreated
          )
      }

    ticks should be > 0
    acted should be > 0
    created should be(1)
  }

  (it should behave).like(
    returningWalker(
      new TestRecruiter(properties, state, modifiers),
      properties.homePosition
    )
  )
}
