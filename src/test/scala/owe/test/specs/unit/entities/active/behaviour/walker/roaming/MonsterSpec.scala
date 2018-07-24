package owe.test.specs.unit.entities.active.behaviour.walker.roaming

import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, WalkerData}
import owe.entities.active.Walker
import owe.entities.active.Walker._
import owe.entities.active.attributes.{Life, Speed}
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker.{DestinationPoint, GoToPoint}
import owe.entities.active.behaviour.walker.roaming.Monster
import owe.map.grid.Point
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures
import owe.test.specs.unit.entities.active.behaviour.walker.WalkerBehaviour

class MonsterSpec extends AkkaUnitSpec("MonsterSpec") with WalkerBehaviour {

  private class TestMonster(
    properties: Properties,
    state: State,
    modifiers: StateModifiers
  ) extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint

    override protected def createBehaviour(): BaseWalker = new Monster {}

    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: WalkerRef =>
        WalkerData(properties, state, modifiers, id)
    }

    override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
  }

  private val properties: Properties = Fixtures.Walker.properties.copy(
    homePosition = Point(0, 0),
    name = "Monster",
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

  ("A Monster walker" should behave).like(
    roamingWalker(
      new TestMonster(properties, state, modifiers)
    )
  )

  (it should behave).like(
    attackingWalker(
      new TestMonster(properties, state, modifiers),
      new TestMonster(properties, state, modifiers)
    )
  )

  (it should behave).like(
    advancingWalker(
      new TestMonster(properties, state, modifiers) {
        override protected def createBehaviour(): BaseWalker = new BaseWalker {
          override protected def behaviour: Receive = advancing(
            mode = MovementMode.Advancing,
            destination = DestinationPoint(Point(2, 2)),
            destinationActions = Seq(GoToPoint(Point(0, 2)))
          )
        }
      },
      firstDestination = Point(2, 2),
      secondDestination = Point(0, 2)
    )
  )
}
