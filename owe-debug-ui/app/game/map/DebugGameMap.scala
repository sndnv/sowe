package game.map

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import owe.Tagging._
import owe.entities.ActiveEntity.ActiveEntityRef
import owe.entities.Entity.EntityRef
import owe.entities.{ActiveEntity, Entity}
import owe.map.Cell.CellData
import owe.map.{Cell, GameMap}
import owe.map.GameMap._
import owe.map.grid.{Grid, Point}
import owe.map.pathfinding.{AStarSearch, Search}
import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{Await, Future}

class DebugGameMap(
  size: Int,
  exchangeRef: ActorRef,
  trackerRef: ActorRef,
  interval: FiniteDuration,
  expiration: FiniteDuration,
  gridUpdates: Map[Point, Seq[Cell.Message]]
) extends GameMap {
  override lazy protected implicit val actionTimeout: Timeout = 3.seconds
  override lazy protected val height: Int = size
  override lazy protected val width: Int = size
  override lazy protected val collectionTimeout: FiniteDuration = expiration
  override lazy protected val tickInterval: FiniteDuration = interval
  override lazy protected val exchange: ActorRef = exchangeRef
  override lazy protected val tracker: ActorRef = trackerRef
  override lazy protected val search: Search = AStarSearch

  grid.foreachIndexed {
    case (point, ref) =>
      gridUpdates.getOrElse(point, Seq.empty).foreach(m => ref ! m)
  }
}

object DebugGameMap {
  trait DebugGameMapRef

  def props(
    size: Int,
    exchangeRef: ActorRef,
    trackerRef: ActorRef,
    interval: FiniteDuration,
    expiration: FiniteDuration,
    gridUpdates: Map[Point, Seq[Cell.Message]]
  ): Props = Props(
    classOf[DebugGameMap],
    size,
    exchangeRef,
    trackerRef,
    interval,
    expiration,
    gridUpdates
  )

  def apply(
    size: Int,
    exchangeRef: ActorRef,
    trackerRef: ActorRef,
    interval: FiniteDuration = 0.seconds,
    expiration: FiniteDuration = 500.millis,
    gridUpdates: Map[Point, Seq[Cell.Message]] = Map.empty
  )(implicit system: ActorSystem): ActorRef @@ DebugGameMapRef =
    system
      .actorOf(
        DebugGameMap.props(size, exchangeRef, trackerRef, interval, expiration, gridUpdates)
      )
      .tag[DebugGameMapRef]

  implicit class RichDebugGameMap(map: ActorRef @@ DebugGameMapRef) {
    def createEntity(entity: Entity, cell: Point)(implicit t: Timeout): EntityRef =
      (map ? CreateEntity(entity, cell)).mapTo[EntityRef].await

    def destroyEntity(entityID: EntityRef): Unit = map ! DestroyEntity(entityID)

    def moveEntity(entityID: EntityRef, cell: Point): Unit = map ! MoveEntity(entityID, cell)

    def getEntity(entityID: ActiveEntityRef)(implicit t: Timeout): ActiveEntity.Data =
      (map ? GetEntity(entityID)).mapTo[ActiveEntity.Data].await

    def getEntityAsync(entityID: ActiveEntityRef)(implicit t: Timeout): Future[ActiveEntity.Data] =
      (map ? GetEntity(entityID)).mapTo[ActiveEntity.Data]

    def grid(implicit t: Timeout): Grid[CellData] = (map ? GetGrid()).mapTo[Grid[CellData]].await
  }

  implicit class RichFuture[T](future: Future[T]) {
    def await(implicit timeout: Timeout): T = Await.result[T](future, atMost = timeout.duration)
  }
}
