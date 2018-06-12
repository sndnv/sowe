package owe.test.specs.unit

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{fixture, BeforeAndAfterAll, Matchers}

abstract class AkkaUnitSpec(systemName: String)
    extends TestKit(ActorSystem(s"AkkaUnitSpec-$systemName"))
    with ImplicitSender
    with fixture.FlatSpecLike
    with Matchers
    with BeforeAndAfterAll {
  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }
}
