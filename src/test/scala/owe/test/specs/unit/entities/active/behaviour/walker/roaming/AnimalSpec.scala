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
import owe.test.specs.unit.entities.active.behaviour.walker.WalkerBehaviour

import scala.collection.immutable.Queue

class AnimalSpec extends AkkaUnitSpec("AnimalSpec") with WalkerBehaviour {

  private class TestAnimal extends Walker {
    override protected def createBehaviour(): BaseWalker = new Animal {}

    override protected def createActiveEntityData(): ActiveEntityRef => Data = {
      case id: WalkerRef =>
        WalkerData(
          properties = Properties(
            parent = None,
            homePosition = Point(0, 0),
            name = "Animal",
            maxLife = Life(500),
            movementSpeed = Speed(1),
            maxRoamingDistance = Distance(50),
            attack = AttackProperties(
              rate = AttackRate(3),
              damage = AttackDamage(50),
              distance = Distance(25),
              target = _ => true
            )
          ),
          state = State(
            currentLife = Life(50),
            distanceCovered = Distance(0),
            commodities = NoCommodities,
            path = Queue.empty,
            mode = MovementMode.Roaming
          ),
          modifiers = StateModifiers(
            movementSpeed = Speed.Modifier(100),
            maxRoamingDistance = Distance.Modifier(100),
            attack = AttackModifiers(
              rate = AttackRate.Modifier(200),
              damage = AttackDamage.Modifier(50),
              distance = Distance.Modifier(100)
            )
          ),
          id
        )
    }

    override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
  }

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  ("An Animal walker" should behave).like(roamingWalker(new TestAnimal))

  (it should behave).like(attackingWalker(new TestAnimal, new TestAnimal))

  it should "follow another walker when captured" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
