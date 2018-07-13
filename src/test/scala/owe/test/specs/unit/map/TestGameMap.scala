package owe.test.specs.unit.map

import scala.concurrent.duration.FiniteDuration

import akka.actor.ActorRef
import akka.util.Timeout
import owe.map.GameMap
import owe.map.pathfinding.{AStarSearch, Search}
import scala.concurrent.duration._

class TestGameMap(ref: ActorRef) extends GameMap {
  override protected implicit val actionTimeout: Timeout = 3.seconds
  override protected val height: Int = 3
  override protected val width: Int = 3
  override protected val tickInterval: FiniteDuration = 0.seconds
  override protected val exchange: ActorRef = ref
  override protected val tracker: ActorRef = ref
  override protected val search: Search = AStarSearch
}
