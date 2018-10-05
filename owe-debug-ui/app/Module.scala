import scala.concurrent.duration._

import akka.actor.ActorSystem
import com.google.inject.{AbstractModule, Provides, Singleton}
import game.GameData
import game.map.DebugGameMap
import owe.events.Tracker
import owe.map.Cell
import owe.map.Cell.UpdateType
import owe.map.grid.Point
import owe.production.Exchange

class Module extends AbstractModule {

  override def configure(): Unit = {}

  @Provides
  @Singleton
  def gameData(implicit system: ActorSystem): GameData = {
    val exchange = system.actorOf(Exchange.props())
    val tracker = system.actorOf(Tracker.props())

    val map = DebugGameMap(
      size = 32,
      exchange,
      tracker,
      interval = 1.second,
      gridUpdates = gridUpdates
    )

    GameData(map, tracker, exchange)
  }

  private val gridUpdates: Map[Point, Seq[Cell.Message]] = Map(
    Point(20, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(21, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(22, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(23, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(24, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(25, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(26, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(27, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(28, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(29, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(30, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 0) -> Seq(UpdateType(Cell.Type.Water)),
    Point(21, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(22, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(23, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(24, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(25, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(26, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(27, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(28, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(29, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(30, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 1) -> Seq(UpdateType(Cell.Type.Water)),
    Point(24, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(25, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(26, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(27, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(28, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(29, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(30, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 2) -> Seq(UpdateType(Cell.Type.Water)),
    Point(24, 3) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(25, 3) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(26, 3) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(27, 3) -> Seq(UpdateType(Cell.Type.Water)),
    Point(28, 3) -> Seq(UpdateType(Cell.Type.Water)),
    Point(29, 3) -> Seq(UpdateType(Cell.Type.Water)),
    Point(30, 3) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 3) -> Seq(UpdateType(Cell.Type.Water)),
    Point(24, 4) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(25, 4) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(26, 4) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(27, 4) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(28, 4) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(29, 4) -> Seq(UpdateType(Cell.Type.Water)),
    Point(30, 4) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 4) -> Seq(UpdateType(Cell.Type.Water)),
    Point(25, 5) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(26, 5) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(27, 5) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(28, 5) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(29, 5) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(30, 5) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 5) -> Seq(UpdateType(Cell.Type.Water)),
    Point(26, 6) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(27, 6) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(28, 6) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(29, 6) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(30, 6) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 6) -> Seq(UpdateType(Cell.Type.Water)),
    Point(27, 7) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(28, 7) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(29, 7) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(30, 7) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 7) -> Seq(UpdateType(Cell.Type.Water)),
    Point(28, 8) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(29, 8) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(30, 8) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 8) -> Seq(UpdateType(Cell.Type.Water)),
    Point(29, 9) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(30, 9) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(31, 9) -> Seq(UpdateType(Cell.Type.Water)),
    Point(30, 10) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(31, 10) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(4, 11) -> Seq(UpdateType(Cell.Type.Water)),
    Point(31, 11) -> Seq(UpdateType(Cell.Type.Floodplain)),
    Point(3, 12) -> Seq(UpdateType(Cell.Type.Water)),
    Point(4, 12) -> Seq(UpdateType(Cell.Type.Water)),
    Point(5, 12) -> Seq(UpdateType(Cell.Type.Water)),
    Point(2, 13) -> Seq(UpdateType(Cell.Type.Water)),
    Point(3, 13) -> Seq(UpdateType(Cell.Type.Water)),
    Point(4, 13) -> Seq(UpdateType(Cell.Type.Water)),
    Point(5, 13) -> Seq(UpdateType(Cell.Type.Water)),
    Point(2, 14) -> Seq(UpdateType(Cell.Type.Water)),
    Point(3, 14) -> Seq(UpdateType(Cell.Type.Water)),
    Point(4, 14) -> Seq(UpdateType(Cell.Type.Water)),
    Point(5, 14) -> Seq(UpdateType(Cell.Type.Water)),
    Point(6, 14) -> Seq(UpdateType(Cell.Type.Water)),
    Point(1, 15) -> Seq(UpdateType(Cell.Type.Water)),
    Point(2, 15) -> Seq(UpdateType(Cell.Type.Water)),
    Point(3, 15) -> Seq(UpdateType(Cell.Type.Water)),
    Point(4, 15) -> Seq(UpdateType(Cell.Type.Water)),
    Point(5, 15) -> Seq(UpdateType(Cell.Type.Water)),
    Point(6, 15) -> Seq(UpdateType(Cell.Type.Water)),
    Point(1, 16) -> Seq(UpdateType(Cell.Type.Water)),
    Point(2, 16) -> Seq(UpdateType(Cell.Type.Water)),
    Point(3, 16) -> Seq(UpdateType(Cell.Type.Water)),
    Point(4, 16) -> Seq(UpdateType(Cell.Type.Water)),
    Point(5, 16) -> Seq(UpdateType(Cell.Type.Water)),
    Point(6, 16) -> Seq(UpdateType(Cell.Type.Water)),
    Point(2, 17) -> Seq(UpdateType(Cell.Type.Water)),
    Point(3, 17) -> Seq(UpdateType(Cell.Type.Water)),
    Point(4, 17) -> Seq(UpdateType(Cell.Type.Water)),
    Point(5, 17) -> Seq(UpdateType(Cell.Type.Water)),
    Point(6, 17) -> Seq(UpdateType(Cell.Type.Water)),
    Point(3, 18) -> Seq(UpdateType(Cell.Type.Water)),
    Point(4, 18) -> Seq(UpdateType(Cell.Type.Water)),
    Point(5, 18) -> Seq(UpdateType(Cell.Type.Water)),
  )
}
