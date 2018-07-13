package owe.test.specs.unit.entities.active

import akka.actor.ActorRef
import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data}
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.active.Resource
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.behaviour.resource.BaseResource
import owe.test.specs.unit.UnitSpec

class ResourceSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  private class UndefinedResource extends Resource {
    override protected def createActiveEntityData(): ActiveEntityRef => Data = ???
    override protected def createEffects(): Seq[(Data => Boolean, Effect)] = ???
    override protected def createBehaviour(): BaseResource = ???

    def createEntityRef(ref: ActorRef): ActiveEntityRef = actorToActiveEntityRef(ref)
  }

  "A Resource" should "have correct basic entity properties" in { _ =>
    val entity = new UndefinedResource
    entity.`size` should be(Entity.Size(height = 1, width = 1))
    entity.`type` should be(Entity.Type.Resource)
    entity.`desirability` should be(Desirability.Neutral)
  }

  it should "create the expected entity actor ref" in { _ =>
    val entity = new UndefinedResource
    val testRef: ActorRef = null
    entity.createEntityRef(testRef) shouldBe a[ResourceRef]
  }
}
