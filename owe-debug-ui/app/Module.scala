import akka.actor.ActorSystem
import com.google.inject.{AbstractModule, Provides, Singleton}
import game.GameData
import game.map.DebugGameMap
import owe.events.Tracker
import owe.production.Exchange

import scala.concurrent.duration._

class Module extends AbstractModule {

  override def configure(): Unit = {}

  @Provides
  @Singleton
  def gameData(implicit system: ActorSystem): GameData = {
    val exchange = system.actorOf(Exchange.props())
    val tracker = system.actorOf(Tracker.props())

    val map = DebugGameMap(size = 32, exchange, tracker, interval = 1.second)

    GameData(map, tracker, exchange)
  }
}
