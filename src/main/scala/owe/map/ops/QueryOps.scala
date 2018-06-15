package owe.map.ops

import akka.pattern.ask
import akka.util.Timeout
import owe.EntityID
import owe.entities.ActiveEntity.{ActiveEntityData, GetData}
import owe.entities.active.Distance
import owe.map._
import owe.map.grid.{Grid, Point}

import scala.collection.immutable.Queue
import scala.concurrent.{ExecutionContext, Future}

trait QueryOps { _: PathingOps =>

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def getAdvancePath(
    grid: Grid[MapCell],
    entities: Map[EntityID, Point],
    entityID: EntityID,
    destination: Point
  ): Option[Queue[Point]] =
    entities.get(entityID).flatMap(start => generateAdvancePath(grid, start, destination))

  def getRoamingPath(
    grid: Grid[MapCell],
    entities: Map[EntityID, Point],
    entityID: EntityID,
    length: Distance
  ): Option[Queue[Point]] =
    entities.get(entityID).flatMap(start => generateRoamPath(grid, start, length))

  def getNeighbours(
    grid: Grid[MapCell],
    entities: Map[EntityID, Point],
    entityID: EntityID,
    radius: Distance
  ): Future[Seq[(EntityID, ActiveEntityData)]] =
    Future
      .traverse(
        entities
          .get(entityID)
          .map { point =>
            grid
              .window(point, radius.value)
              .toSeq
              .flatMap { mapCell =>
                mapCell.entities.collect {
                  case (k, v: ActiveMapEntity) => (k, (v.entity ? GetData()).mapTo[ActiveEntityData])
                }
              }
              .toMap
          }
          .getOrElse(Map.empty)
      ) {
        case (neighbourEntityID, future) =>
          future.map { result =>
            (neighbourEntityID, result)
          }
      }
      .map(_.toSeq)

  def getEntities(
    grid: Grid[MapCell],
    entities: Map[EntityID, Point],
    point: Point
  ): Future[Seq[(MapEntity, Option[ActiveEntityData])]] =
    Future
      .sequence(
        grid
          .get(point)
          .map { mapCell =>
            mapCell.entities.values.toSeq.map {
              case entity: ActiveMapEntity =>
                (entity.entity ? GetData()).mapTo[ActiveEntityData].map(data => (entity, Some(data)))

              case entity: PassiveMapEntity =>
                Future.successful(entity, None)
            }
          }
          .getOrElse(Seq.empty)
      )

  def getEntity(
    grid: Grid[MapCell],
    entities: Map[EntityID, Point],
    entityID: EntityID
  ): Future[ActiveEntityData] =
    entities
      .get(entityID)
      .flatMap(grid.get)
      .flatMap(_.entities.get(entityID))
      .collect {
        case ActiveMapEntity(entity, _, _, _) =>
          entity ? GetData()
      } match {
      case Some(result) =>
        result.mapTo[ActiveEntityData]

      case None =>
        val message = s"Failed to find entity with ID [$entityID] while retrieving its data."
        Future.failed(new IllegalStateException(message))
    }
}
