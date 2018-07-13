package owe.test.specs.unit.entities.active

import akka.actor.ActorRef
import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data}
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.active.Structure
import owe.entities.active.Structure.StructureRef
import owe.entities.active.behaviour.structure.BaseStructure
import owe.test.specs.unit.UnitSpec

class StructureSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  private class UndefinedStructure extends Structure {
    override protected def createActiveEntityData(): ActiveEntityRef => Data = ???
    override protected def createEffects(): Seq[(Data => Boolean, Effect)] = ???
    override protected def createBehaviour(): BaseStructure = ???
    override def `size`: Entity.Size = Entity.Size(height = 7, width = 3)
    override def `desirability`: Desirability = Desirability.Max

    def createEntityRef(ref: ActorRef): ActiveEntityRef = actorToActiveEntityRef(ref)
  }

  "A Structure" should "have correct basic entity properties" in { _ =>
    val entity = new UndefinedStructure
    entity.`size` should be(Entity.Size(height = 7, width = 3))
    entity.`type` should be(Entity.Type.Structure)
    entity.`desirability` should be(Desirability.Max)
  }

  it should "create the expected entity actor ref" in { _ =>
    val entity = new UndefinedStructure
    val testRef: ActorRef = null
    entity.createEntityRef(testRef) shouldBe a[StructureRef]
  }
}
