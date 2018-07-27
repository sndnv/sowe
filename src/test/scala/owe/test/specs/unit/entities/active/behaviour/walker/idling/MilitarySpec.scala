package owe.test.specs.unit.entities.active.behaviour.walker.idling

import scala.collection.immutable.Queue

import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, WalkerData}
import owe.entities.active.Walker
import owe.entities.active.Walker._
import owe.entities.active.attributes.{Life, Speed}
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker.{DestinationPoint, Idle}
import owe.entities.active.behaviour.walker.idling.Military
import owe.map.grid.Point
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures
import owe.test.specs.unit.entities.active.behaviour.walker.WalkerBehaviour

class MilitarySpec extends AkkaUnitSpec("MilitarySpec") with WalkerBehaviour {

  private class TestMilitary(
    properties: Properties,
    state: State,
    modifiers: StateModifiers
  ) extends Walker {
    override def spawnLocation: SpawnLocation = SpawnLocation.AtPoint

    override protected def createBehaviour(): BaseWalker = new Military {}

    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: WalkerRef =>
        WalkerData(properties, state, modifiers, id)
    }

    override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
  }

  private val properties: Properties = Fixtures.Walker.properties.copy(
    homePosition = Point(0, 0),
    name = "Military",
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

  ("A Military walker" should behave).like(
    advancingWalker(
      new TestMilitary(properties, state, modifiers) {
        override protected def createBehaviour(): BaseWalker = new BaseWalker {
          override protected def behaviour: Receive = advancing(
            mode = MovementMode.Advancing,
            destination = DestinationPoint(Point(2, 2)),
            destinationActions = Seq(Idle())
          )
        }
      },
      destination = Point(2, 2),
      action = Idle()
    )
  )

  (it should behave).like(
    attackingWalker(
      new TestMilitary(properties, state, modifiers),
      new TestMilitary(properties, state, modifiers)
    )
  )

  (it should behave).like(
    followingWalker(
      followingWalker = new TestMilitary(
        properties.copy(attack = NoAttack),
        state.copy(
          currentPosition = (0, 0),
          path = Queue.empty
        ),
        modifiers.copy(attack = NoAttack)
      ),
      followedWalker = new TestMilitary(
        properties.copy(attack = NoAttack),
        state.copy(
          currentPosition = (1, 0),
          path = Queue[Point]((2, 0), (2, 1), (2, 2)),
          mode = MovementMode.Advancing
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
      expectedFollowedPath = Queue[Point]((2, 0), (2, 1), (2, 2))
    )
  )
}
