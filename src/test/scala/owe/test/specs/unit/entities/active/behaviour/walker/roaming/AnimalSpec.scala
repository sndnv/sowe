package owe.test.specs.unit.entities.active.behaviour.walker.roaming

import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, WalkerData}
import owe.entities.active.Walker
import owe.entities.active.Walker._
import owe.entities.active.attributes._
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.roaming.Animal
import owe.map.grid.Point
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures
import owe.test.specs.unit.entities.active.behaviour.walker.WalkerBehaviour

class AnimalSpec extends AkkaUnitSpec("AnimalSpec") with WalkerBehaviour {

  private class TestAnimal(
    properties: Properties,
    state: State,
    modifiers: StateModifiers
  ) extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint

    override protected def createBehaviour(): BaseWalker = new Animal {}

    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: WalkerRef =>
        WalkerData(properties, state, modifiers, id)
    }

    override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
  }

  private val properties: Properties = Fixtures.Walker.properties.copy(
    homePosition = Point(0, 0),
    name = "Animal",
    movementSpeed = Speed(1)
  )

  private val state: State = Fixtures.Walker.state.copy(
    currentLife = Life(50),
    commodities = NoCommodities,
    mode = MovementMode.Roaming
  )

  private val modifiers: StateModifiers = Fixtures.Walker.modifiers

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  ("An Animal walker" should behave).like(
    roamingWalker(
      new TestAnimal(properties, state, modifiers)
    )
  )

  (it should behave).like(
    attackingWalker(
      new TestAnimal(properties, state, modifiers),
      new TestAnimal(properties, state, modifiers)
    )
  )

  (it should behave).like(
    followingWalker(
      new TestAnimal(properties, state, modifiers)
    )
  )
}
