package owe.test.specs.unit.entities.active.behaviour.walker.roaming

import scala.collection.immutable.Queue

import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, WalkerData}
import owe.entities.active.Walker
import owe.entities.active.Walker._
import owe.entities.active.attributes.{Life, Speed}
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker.{DestinationPoint, GoToPoint}
import owe.entities.active.behaviour.walker.roaming.Deity
import owe.map.grid.Point
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures
import owe.test.specs.unit.entities.active.behaviour.walker.WalkerBehaviour

class DeitySpec extends AkkaUnitSpec("DeitySpec") with WalkerBehaviour {

  private class TestDeity(
    properties: Properties,
    state: State,
    modifiers: StateModifiers
  ) extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint

    override protected def createBehaviour(): BaseWalker = new Deity {}

    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: WalkerRef =>
        WalkerData(properties, state, modifiers, id)
    }

    override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
  }

  private val properties: Properties = Fixtures.Walker.properties.copy(
    homePosition = Point(0, 0),
    name = "Deity",
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

  ("A Deity walker" should behave).like(
    roamingWalker(
      new TestDeity(properties, state, modifiers)
    )
  )

  (it should behave).like(
    attackingWalker(
      new TestDeity(properties, state, modifiers),
      new TestDeity(properties, state, modifiers)
    )
  )

  (it should behave).like(
    advancingWalker(
      new TestDeity(properties, state, modifiers) {
        override protected def createBehaviour(): BaseWalker = new BaseWalker {
          override protected def behaviour: Receive = advancing(
            mode = MovementMode.Advancing,
            destination = DestinationPoint(Point(2, 2)),
            destinationActions = Seq(GoToPoint(Point(0, 2)))
          )
        }
      },
      destination = Point(2, 2),
      action = GoToPoint(0, 2)
    )
  )

  (it should behave).like(
    followingWalker(
      followedWalker = new TestDeity(
        properties.copy(attack = NoAttack),
        state.copy(
          currentPosition = (1, 0),
          path = Queue[Point]((2, 0), (2, 1), (2, 2))
        ),
        modifiers.copy(attack = NoAttack)
      ) {
        override protected def createBehaviour(): BaseWalker = new BaseWalker {
          override protected def behaviour: Behaviour = advancing(
            mode = MovementMode.Advancing,
            destination = DestinationPoint((2, 2)),
            destinationActions = Seq.empty
          )
        }
      },
      followingWalker = new TestDeity(
        properties.copy(attack = NoAttack),
        state.copy(
          currentPosition = (0, 0),
          path = Queue.empty
        ),
        modifiers.copy(attack = NoAttack)
      ),
      expectedFollowedPath = Queue[Point]((2, 0), (2, 1), (2, 2))
    )
  )
}
