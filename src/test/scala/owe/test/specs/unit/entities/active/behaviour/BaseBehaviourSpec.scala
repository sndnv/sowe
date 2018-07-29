package owe.test.specs.unit.entities.active.behaviour

import akka.actor.Props
import akka.pattern.pipe
import org.scalatest.Outcome
import owe.entities.ActiveEntity.ResourceData
import owe.entities.active.Resource
import owe.entities.active.behaviour.resource.BaseResource
import owe.production.Commodity
import owe.test.specs.unit.AkkaUnitSpec

import scala.concurrent.Future

class BaseBehaviourSpec extends AkkaUnitSpec("BaseBehaviourSpec") {

  private case class ApplyAsyncUpdates(data: ResourceData, updates: Seq[ResourceData => Future[Resource.State]])
  private case class ApplyUpdates(data: ResourceData, updates: Seq[ResourceData => Resource.State])
  private case class ApplyResult(data: ResourceData)

  private class TestBehaviour extends BaseResource {
    import system.dispatcher

    override protected def base: Behaviour = new Receive {
      override def apply(any: Any): Unit = {}
      override def isDefinedAt(any: Any) = false
    }

    override protected def behaviour: Behaviour = {
      case ApplyUpdates(data, updates) =>
        withUpdates(data, updates).pipeTo(sender)

      case ApplyAsyncUpdates(data, updates) =>
        withAsyncUpdates(data, updates).pipeTo(sender)
    }
  }

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A BaseBehaviour" should "update entity data" in { _ =>
    val behaviour = system.actorOf(Props(new TestBehaviour))

    val data = ResourceData(
      Fixtures.Resource.properties,
      Fixtures.Resource.state,
      Fixtures.Resource.modifiers,
      Fixtures.MockRefs.resource
    )

    behaviour ! ApplyAsyncUpdates(
      data,
      updates = Seq.empty
    )
    expectMsg(data)

    behaviour ! ApplyAsyncUpdates(
      data,
      updates = Seq(
        data => Future.successful(data.state.copy(currentAmount = Commodity.Amount(42)))
      )
    )
    expectMsg(
      data.copy(
        state = data.state.copy(currentAmount = Commodity.Amount(42))
      )
    )

    behaviour ! ApplyAsyncUpdates(
      data,
      updates = Seq(
        data => Future.successful(data.state.copy(currentAmount = Commodity.Amount(42))),
        data => Future.successful(data.state.copy(replenishAmount = Commodity.Amount(9000)))
      )
    )
    expectMsg(
      data.copy(
        state = data.state.copy(
          currentAmount = Commodity.Amount(42),
          replenishAmount = Commodity.Amount(9000)
        )
      )
    )

    behaviour ! ApplyUpdates(
      data,
      updates = Seq(
        data => data.state.copy(currentAmount = Commodity.Amount(42)),
        data => data.state.copy(replenishAmount = Commodity.Amount(9000))
      )
    )
    expectMsg(
      data.copy(
        state = data.state.copy(
          currentAmount = Commodity.Amount(42),
          replenishAmount = Commodity.Amount(9000)
        )
      )
    )
  }
}
