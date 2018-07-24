package owe.test.specs.unit.map

import akka.actor.ActorRef
import akka.util.Timeout
import owe.map.GameMap
import owe.map.pathfinding.{AStarSearch, Search}
import owe.test.specs.unit.map.TestGameMap.StartBehaviour

import scala.concurrent.duration.{FiniteDuration, _}

class TestGameMap(
  ref: ActorRef,
  startBehaviour: StartBehaviour,
  interval: FiniteDuration = 0.seconds,
  expiration: FiniteDuration = 500.millis
) extends GameMap {
  override lazy protected implicit val actionTimeout: Timeout = 3.seconds
  override lazy protected val height: Int = 3
  override lazy protected val width: Int = 3
  override lazy protected val collectionTimeout: FiniteDuration = expiration
  override lazy protected val tickInterval: FiniteDuration = interval
  override lazy protected val exchange: ActorRef = ref
  override lazy protected val tracker: ActorRef = ref
  override lazy protected val search: Search = AStarSearch

  override def receive: Receive = startBehaviour match {
    case StartBehaviour.Active  => active(entities = Map.empty, currentTick = 0)
    case StartBehaviour.Waiting => waiting(entities = Map.empty, currentTick = 0, pendingEntityResponses = 0)
    case StartBehaviour.Idle    => super.receive
  }
}

object TestGameMap {
  sealed trait StartBehaviour
  object StartBehaviour {
    case object Active extends StartBehaviour
    case object Idle extends StartBehaviour
    case object Waiting extends StartBehaviour
  }
}
