package owe.map.ops

import akka.pattern.ask
import akka.util.Timeout
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.{ActiveEntityActorRef, ActiveEntityData, GetData}
import owe.entities.Entity.EntityActorRef
import owe.entities.active.{Distance, Walker}
import owe.map.Cell.{CellActorRef, CellData, GetCellData, GetEntity}
import owe.map._
import owe.map.grid.{Grid, Point}
import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}

import owe.entities.active.Walker.WalkerActorRef

trait QueryOps { _: PathfindingOps =>

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def getAdvancePath(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    entityID: WalkerActorRef,
    destination: Point
  ): Future[Queue[Point]] =
    entities
      .get(entityID)
      .map(start => generateAdvancePath(grid, start, destination))
      .getOrElse(Future.successful(Queue.empty))

  def getRoamingPath(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    entityID: WalkerActorRef,
    length: Distance
  ): Future[Queue[Point]] =
    entities
      .get(entityID)
      .map(start => generateRoamPath(grid, start, length))
      .getOrElse(Future.successful(Queue.empty))

  def getNeighbours(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    entityID: EntityActorRef,
    radius: Distance
  ): Future[Seq[(EntityActorRef, ActiveEntityData)]] =
    entities
      .get(entityID)
      .map { point =>
        Future
          .sequence(
            grid
              .window(point, radius.value)
              .toSeq
              .map { mapCell =>
                (mapCell ? GetCellData()).mapTo[CellData].flatMap { cellData =>
                  Future
                    .sequence(
                      cellData.entities.toSeq.collect {
                        case (id, MapEntity(entity: ActiveEntityActorRef, _, _, _)) =>
                          (entity ? GetData()).mapTo[ActiveEntityData].map(data => (id, data))
                      }
                    )
                }
              }
          )
          .map(_.flatten)
      }
      .getOrElse(Future.successful(Seq.empty))

  def getEntities(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    point: Point
  ): Future[Seq[(MapEntity, Option[ActiveEntityData])]] =
    grid
      .get(point)
      .map { mapCell =>
        (mapCell ? GetCellData()).mapTo[CellData].flatMap { cellData =>
          Future.sequence(
            cellData.entities.values.toSeq.map {
              case mapEntity @ MapEntity(entity: ActiveEntityActorRef, _, _, _) =>
                (entity ? GetData()).mapTo[ActiveEntityData].map(data => (mapEntity, Some(data)))

              case entity =>
                Future.successful(entity, None)
            }
          )
        }
      }
      .getOrElse(Future.successful(Seq.empty))

  def getEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    entityID: EntityActorRef
  ): Future[ActiveEntityData] =
    entities
      .get(entityID)
      .flatMap(grid.get)
      .map { cell =>
        (cell ? GetEntity(entityID))
          .mapTo[MapEntity]
          .collect {
            case MapEntity(entity, _, _, _) =>
              (entity ? GetData()).mapTo[ActiveEntityData]
          }
          .flatten
      } match {
      case Some(result) =>
        result

      case None =>
        val message = s"Failed to find entity with ID [$entityID] while retrieving its data."
        Future.failed(new IllegalStateException(message))
    }
}
