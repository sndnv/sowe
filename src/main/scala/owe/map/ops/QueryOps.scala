package owe.map.ops

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import owe.entities.ActiveEntity.{ActiveEntityRef, Data}
import owe.entities.ActiveEntityActor.GetData
import owe.entities.Entity.EntityRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.attributes.Distance
import owe.map.Cell.{CellActorRef, CellData, GetCellData, GetEntity}
import owe.map._
import owe.map.grid.{Grid, Point}

import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}

trait QueryOps { _: PathfindingOps =>

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def getAdvancePath(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: WalkerRef,
    destination: Point
  )(implicit sender: ActorRef = Actor.noSender): Future[Queue[Point]] =
    entities
      .get(entityID)
      .map(start => generateAdvancePath(grid, start, destination))
      .getOrElse(Future.successful(Queue.empty))

  def getRoamingPath(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: WalkerRef,
    length: Distance
  )(implicit sender: ActorRef = Actor.noSender): Future[Queue[Point]] =
    entities
      .get(entityID)
      .map(start => generateRoamPath(grid, start, length))
      .getOrElse(Future.successful(Queue.empty))

  def getNeighbours(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef,
    radius: Distance
  )(implicit sender: ActorRef = Actor.noSender): Future[Seq[(EntityRef, Data)]] =
    entities
      .get(entityID)
      .map { point =>
        Future
          .sequence(
            grid
              .window(point, radius.value)
              .toSeq
              .map { mapCell =>
                (mapCell ? GetCellData()).mapTo[CellData].map { cellData =>
                  cellData.entities.values.collect {
                    case MapEntity(entityRef: ActiveEntityRef, _, _, _) if entityID != entityRef =>
                      entityRef
                  }
                }
              }
          )
          .map { refs =>
            Future.sequence(
              refs.flatten.distinct.map { ref =>
                (ref ? GetData()).mapTo[Data].map(data => (ref, data))
              }
            )
          }
          .flatten
      }
      .getOrElse(Future.successful(Seq.empty))

  def getEntities(
    grid: Grid[CellActorRef],
    point: Point
  )(implicit sender: ActorRef = Actor.noSender): Future[Seq[(MapEntity, Option[Data])]] =
    grid
      .get(point)
      .map { mapCell =>
        (mapCell ? GetCellData())
          .mapTo[CellData]
          .map { cellData =>
            cellData.entities.values.collect {
              case mapEntity @ MapEntity(entityRef: ActiveEntityRef, _, _, _) =>
                (mapEntity, entityRef)
            }
          }
          .map { refs =>
            Future.sequence(
              refs.toSeq.distinct.map {
                case (mapEntity, ref) =>
                  (ref ? GetData()).mapTo[Data].map(data => (mapEntity, Some(data)))
              }
            )
          }
          .flatten
      }
      .getOrElse(Future.successful(Seq.empty))

  def getEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef
  )(implicit sender: ActorRef = Actor.noSender): Future[Data] =
    entities
      .get(entityID)
      .flatMap(grid.get)
      .map { cell =>
        (cell ? GetEntity(entityID))
          .mapTo[Option[MapEntity]]
          .collect {
            case Some(MapEntity(entity: ActiveEntityRef, _, _, _)) =>
              (entity ? GetData()).mapTo[Data]
          }
          .flatten
      } match {
      case Some(result) =>
        result

      case None =>
        val message = s"Failed to find entity with ID [$entityID] while retrieving its data"
        Future.failed(new IllegalStateException(message))
    }
}
