package owe.map.ops

import akka.pattern.ask
import akka.util.Timeout
import owe.EntityID
import owe.entities.ActiveEntity.{ActiveEntityData, GetData}
import owe.entities.active.Distance
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
    entities: Map[EntityID, Point],
    entityID: EntityID,
    destination: Point
  ): Future[Queue[Point]] =
    entities
      .get(entityID)
      .map(start => generateAdvancePath(grid, start, destination))
      .getOrElse(Future.successful(Queue.empty))

  def getRoamingPath(
    grid: Grid[CellActorRef],
    entities: Map[EntityID, Point],
    entityID: EntityID,
    length: Distance
  ): Future[Queue[Point]] =
    entities
      .get(entityID)
      .map(start => generateRoamPath(grid, start, length))
      .getOrElse(Future.successful(Queue.empty))

  def getNeighbours(
    grid: Grid[CellActorRef],
    entities: Map[EntityID, Point],
    entityID: EntityID,
    radius: Distance
  ): Future[Seq[(EntityID, ActiveEntityData)]] =
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
                        case (id, entity: ActiveMapEntity) =>
                          (entity.entity ? GetData()).mapTo[ActiveEntityData].map(data => (id, data))
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
    entities: Map[EntityID, Point],
    point: Point
  ): Future[Seq[(MapEntity, Option[ActiveEntityData])]] =
    grid
      .get(point)
      .map { mapCell =>
        (mapCell ? GetCellData()).mapTo[CellData].flatMap { cellData =>
          Future.sequence(
            cellData.entities.values.toSeq.map {
              case entity: ActiveMapEntity =>
                (entity.entity ? GetData()).mapTo[ActiveEntityData].map(data => (entity, Some(data)))

              case entity: PassiveMapEntity =>
                Future.successful(entity, None)
            }
          )
        }
      }
      .getOrElse(Future.successful(Seq.empty))

  def getEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityID, Point],
    entityID: EntityID
  ): Future[ActiveEntityData] =
    entities
      .get(entityID)
      .flatMap(grid.get)
      .map { cell =>
        (cell ? GetEntity(entityID))
          .mapTo[MapEntity]
          .collect {
            case ActiveMapEntity(entity, _, _, _) =>
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
