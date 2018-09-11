package game

import akka.actor.ActorRef
import game.map.DebugGameMap.DebugGameMapRef
import owe.Tagging.@@

case class GameData(
  map: ActorRef @@ DebugGameMapRef,
  tracker: ActorRef,
  exchange: ActorRef
)
