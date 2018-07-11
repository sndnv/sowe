package owe.test.specs.unit.entities.active

import akka.actor.ActorRef
import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, ActiveEntityRef}
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.active.Walker
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.behaviour.walker.BaseWalker
import owe.test.specs.unit.UnitSpec

class WalkerSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  private class UndefinedWalker extends Walker {
    override protected def createActiveEntityData(): ActiveEntityRef => ActiveEntityData = ???
    override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = ???
    override protected def createBehaviour(): BaseWalker = ???

    def createEntityRef(ref: ActorRef): ActiveEntityRef = actorToActiveEntityRef(ref)
  }

  "A Walker" should "have correct basic entity properties" in { _ =>
    val entity = new UndefinedWalker {}
    entity.`size` should be(Entity.Size(height = 1, width = 1))
    entity.`type` should be(Entity.Type.Walker)
    entity.`desirability` should be(Desirability.Neutral)
  }

  it should "create the expected entity actor ref" in { _ =>
    val entity = new UndefinedWalker
    val testRef: ActorRef = null
    entity.createEntityRef(testRef) shouldBe a[WalkerRef]
  }
}
