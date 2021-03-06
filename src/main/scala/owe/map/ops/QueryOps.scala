package owe.map.ops

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, WalkerData}
import owe.entities.ActiveEntityActor.GetData
import owe.entities.Entity.EntityRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.attributes.Distance
import owe.map.Cell.{CellActorRef, CellData, GetCellData, GetEntity}
import owe.map._
import owe.map.grid.{Grid, Point}

import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait QueryOps { _: PathfindingOps =>

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def getAdvancePath(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: WalkerRef,
    destination: Point
  )(implicit sender: ActorRef): Future[Queue[Point]] =
    entities
      .get(entityID)
      .map { start =>
        (entityID ? GetData()).mapTo[WalkerData].flatMap { walkerData =>
          generateAdvancePath(grid, start, destination, walkerData.properties.traversalMode)
        }
      }
      .getOrElse(Future.successful(Queue.empty))

  def getRoamingPath(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: WalkerRef,
    length: Distance
  )(implicit sender: ActorRef): Future[Queue[Point]] =
    entities
      .get(entityID)
      .map { start =>
        (entityID ? GetData()).mapTo[WalkerData].flatMap { walkerData =>
          generateRoamPath(grid, start, length, walkerData.properties.traversalMode)
        }
      }
      .getOrElse(Future.successful(Queue.empty))

  def getNeighbours(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef,
    radius: Distance
  )(implicit sender: ActorRef): Future[Seq[(EntityRef, Data)]] =
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
                    case MapEntity(entityRef: ActiveEntityRef, _, _) if entityID != entityRef =>
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
  )(implicit sender: ActorRef): Future[Seq[(MapEntity, Option[Data])]] =
    grid
      .get(point)
      .map { mapCell =>
        (mapCell ? GetCellData())
          .mapTo[CellData]
          .map { cellData =>
            cellData.entities.values.collect {
              case mapEntity @ MapEntity(entityRef: ActiveEntityRef, _, _) =>
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
  )(implicit sender: ActorRef): Future[Data] =
    entities
      .get(entityID)
      .flatMap(grid.get)
      .map { cell =>
        (cell ? GetEntity(entityID))
          .mapTo[Option[MapEntity]]
          .collect {
            case Some(MapEntity(entity: ActiveEntityRef, _, _)) =>
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

  def getGridData(grid: Grid[CellActorRef]): Future[Grid[CellData]] =
    Future
      .traverse(grid.map(ref => (ref ? GetCellData()).mapTo[CellData]).toMap) {
        case (point, future) => future.map(cell => (point, cell))
      }
      .flatMap { seq =>
        grid.rebuilt(seq.toMap) match {
          case Success(data) => Future.successful(data)
          case Failure(e)    => Future.failed(e)
        }
      }
}
