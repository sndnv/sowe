package owe.test.specs.unit.entities

import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.ActiveEntityActor.GetData
import owe.entities.{Entity, PassiveEntity}
import owe.test.specs.unit.AkkaUnitSpec

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class PassiveEntitySpec extends AkkaUnitSpec("PassiveEntitySpec") {
  private implicit val timeout: Timeout = 3.seconds

  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Passive Entity" should "fail to process incoming messages" in { _ =>
    val entity = new PassiveEntity {
      override def `type`: Entity.Type = Entity.Type.Doodad

      override def `desirability`: Entity.Desirability = Entity.Desirability.Min
    }

    val entityActor = system.actorOf(entity.props())
    entityActor ! GetData()

    val response = receiveOne(timeout.duration).asInstanceOf[Failure[IllegalStateException]]
    response.exception shouldBe an[IllegalStateException]
    response.exception.getMessage should be("Passive entity received message [GetData()]!")
  }
}
