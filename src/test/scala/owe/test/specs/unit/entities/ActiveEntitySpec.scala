package owe.test.specs.unit.entities

import akka.actor.ActorRef
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.ResourceData
import owe.entities.active.Resource
import owe.entities.active.Resource.{Properties, ResourceRef, State, StateModifiers}
import owe.entities.active.behaviour.resource.BaseResource
import owe.map.grid.Point
import owe.production.Commodity
import owe.test.specs.unit.AkkaUnitSpec

import scala.concurrent.duration._

class ActiveEntitySpec extends AkkaUnitSpec("ActiveEntitySpec") {
  private implicit val timeout: Timeout = 3.seconds

  private class TestEntity(ref: ActorRef) extends Resource {
    override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = {
      case resourceRef: ResourceRef =>
        ResourceData(
          properties = Properties(
            name = "TestResource",
            homePosition = Point(0, 0),
            commodity = Commodity("TestCommodity"),
            maxAmount = Commodity.Amount(500)
          ),
          state = State(
            currentAmount = Commodity.Amount(100),
            replenishAmount = Commodity.Amount(25)
          ),
          modifiers = StateModifiers(
            replenishAmount = Commodity.AmountModifier(100)
          ),
          id = resourceRef
        )

    }

    override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, Effect)] = Seq.empty

    override protected def createBehaviour(): BaseResource = new BaseResource {
      override protected def behaviour: Receive = {
        case message => ref.forward(message)
      }
    }
  }

  case class FixtureParam(entityActor: ActorRef)

  def withFixture(test: OneArgTest): Outcome = {
    val entity = new TestEntity(testActor)
    val entityActor = system.actorOf(entity.props())

    withFixture(test.toNoArgTest(FixtureParam(entityActor)))
  }

  "An Active Entity" should "update entity state when active" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "forward messages to parent when active" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "respond with entity data when active" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "it should stash unsupported messages when active" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "apply entity effects when idle" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "become active to process game ticks when idle" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "respod with active entity effects when idle" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "forward messages to parent when idle" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "add entity messages when idle" in { fixture =>
    fail("Not Implemented", new NotImplementedError())
  }
}
