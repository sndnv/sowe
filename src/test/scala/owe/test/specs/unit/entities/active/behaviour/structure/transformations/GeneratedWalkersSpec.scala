package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import akka.testkit.TestActors
import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.{ActiveEntityRef, StructureData}
import owe.entities.ActiveEntityActor.ForwardMessage
import owe.entities.active.Structure.{StructureRef, WalkerState, WalkersProperties, WalkersState}
import owe.entities.active.Walker
import owe.entities.active.Walker.SpawnLocation
import owe.entities.active.behaviour.structure.transformations.GeneratedWalkers
import owe.entities.active.behaviour.walker.BaseWalker
import owe.map.GameMap.CreateEntity
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

import scala.concurrent.duration._

class GeneratedWalkersSpec extends AkkaUnitSpec("GeneratedWalkersSpec") {

  case class FixtureParam(transformer: GeneratedWalkers, structure: StructureData)

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new GeneratedWalkers {
      override val parentEntity: StructureRef =
        StructureRef(system.actorOf(TestActors.forwardActorProps(testActor)))
    }

    val structure = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state,
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    withFixture(test.toNoArgTest(FixtureParam(transformer, structure)))
  }

  private case class DummyWalker() extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint
    override protected def createActiveEntityData(): ActiveEntityRef => ActiveEntity.Data = ???
    override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, Effect)] = ???
    override protected def createBehaviour(): BaseWalker = ???
  }

  private def dummyWalkerGenerator(structure: StructureData): Option[Walker] = Some(new DummyWalker)

  private def noWalkerGenerator(structure: StructureData): Option[Walker] = None

  "A GeneratedWalkers transformation" should "create new walkers" in { fixture =>
    {
      val structureWithWalkers = fixture.structure.copy(
        properties = fixture.structure.properties.copy(
          walkers = WalkersProperties(
            generators = Map(
              "dummy" -> dummyWalkerGenerator
            )
          )
        ),
        state = fixture.structure.state.copy(
          walkers = WalkersState(
            state = Map.empty
          )
        )
      )

      fixture.transformer.withGeneratedWalkers(
        structureWithWalkers
      ) should be(
        structureWithWalkers.state.copy(
          walkers = WalkersState(
            state = Map("dummy" -> WalkerState.Available)
          )
        )
      )
    }

    expectMsg(
      ForwardMessage(
        CreateEntity(new DummyWalker, fixture.structure.properties.homePosition)
      )
    )

    fixture.transformer.withGeneratedWalkers(
      fixture.structure
    ) should be(fixture.structure.state)

    expectNoMessage(max = 500.milliseconds)

    {
      val structureWithNoGeneratedWalkers = fixture.structure.copy(
        properties = fixture.structure.properties.copy(
          walkers = WalkersProperties(
            generators = Map(
              "none" -> noWalkerGenerator
            )
          )
        ),
        state = fixture.structure.state.copy(
          walkers = WalkersState(
            state = Map.empty
          )
        )
      )

      fixture.transformer.withGeneratedWalkers(
        structureWithNoGeneratedWalkers
      ) should be(
        structureWithNoGeneratedWalkers.state
      )
    }

    expectNoMessage(max = 500.milliseconds)

    {
      val structureWithGeneratedWalkers = fixture.structure.copy(
        properties = fixture.structure.properties.copy(
          walkers = WalkersProperties(
            generators = Map(
              "dummy" -> dummyWalkerGenerator
            )
          )
        ),
        state = fixture.structure.state.copy(
          walkers = WalkersState(
            state = Map("dummy" -> WalkerState.Available)
          )
        )
      )

      fixture.transformer.withGeneratedWalkers(
        structureWithGeneratedWalkers
      ) should be(
        structureWithGeneratedWalkers.state
      )
    }

    expectNoMessage(max = 500.milliseconds)
  }
}
