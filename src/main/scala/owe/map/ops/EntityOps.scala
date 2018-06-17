package owe.map.ops

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import owe.EntityDesirability
import owe.Tagging._
import owe.entities.Entity
import owe.entities.Entity.EntityActorRef
import owe.entities.active.Structure
import owe.entities.passive.{Doodad, Road}
import owe.events.Event
import owe.map.Cell._
import owe.map._
import owe.map.grid.{Grid, Point}

import scala.concurrent.{ExecutionContext, Future}

trait EntityOps { _: AvailabilityOps =>

  protected val tracker: ActorRef

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def createEntity[T <: Entity.ActorRefTag](
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    entity: Entity[T],
    cell: Point,
    actorFactory: Props => ActorRef
  ): Future[Map[EntityActorRef, Point]] =
    grid.get(cell) match {
      case Some(mapCell) =>
        val targetAvailability = requiredAvailability(entity.`type`)
        cellAvailability(mapCell).flatMap { availability =>
          if (availability == targetAvailability) {
            Future
              .sequence(Entity.cells(entity.`size`, cell).map(cellAvailability(grid, _)))
              .map { cellsAvailability =>
                if (cellsAvailability.forall(_ == targetAvailability)) {
                  val mapEntity = MapEntity(
                    actorFactory(entity.props()).tag[entity.Tag],
                    cell,
                    entity.`size`,
                    entity.`desirability`
                  )

                  tracker ! Event(Event.System.EntityCreated, Some(cell))
                  associateMapEntity(grid, entities, mapEntity, cell)
                } else {
                  tracker ! Event(Event.System.CellsUnavailable, Some(cell))
                  entities
                }
              }
          } else {
            tracker ! Event(Event.System.CellsUnavailable, Some(cell))
            Future.successful(entities)
          }
        }

      case None =>
        tracker ! Event(Event.System.CellOutOfBounds, Some(cell))
        Future.successful(entities)
    }

  def destroyEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    entityID: EntityActorRef
  ): Future[Map[EntityActorRef, Point]] =
    entities
      .get(entityID)
      .flatMap { point =>
        grid.get(point).map(mapCell => (point, mapCell))
      } match {
      case Some((cell, mapCell)) =>
        cellAvailability(mapCell).flatMap {
          case Availability.Occupied | Availability.Passable =>
            (mapCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].map {
              case Some(mapEntity) =>
                tracker ! Event(Event.System.EntityDestroyed, Some(cell))
                dissociateMapEntity(grid, entities, mapEntity, cell)

              case None =>
                tracker ! Event(Event.System.EntityMissing, Some(cell))
                entities
            }

          case _ =>
            tracker ! Event(Event.System.EntityMissing, Some(cell))
            Future.successful(entities)
        }

      case None =>
        tracker ! Event(Event.System.CellOutOfBounds, cell = None)
        Future.successful(entities)
    }

  def moveEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    entityID: EntityActorRef,
    newCell: Point
  ): Future[Map[EntityActorRef, Point]] = {

    val result = for {
      currentCell <- entities
        .get(entityID)
        .toRight(Event(Event.System.CellsUnavailable, cell = None)): Either[Event, Point]
      currentMapCell <- grid
        .get(currentCell)
        .toRight(Event(Event.System.EntityMissing, Some(currentCell))): Either[Event, CellActorRef]
      newMapCell <- grid
        .get(newCell)
        .toRight(Event(Event.System.CellsUnavailable, Some(newCell))): Either[Event, CellActorRef]
    } yield {
      (currentMapCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].flatMap {
        case Some(currentMapEntity) =>
          val targetAvailability = requiredAvailability(currentMapEntity.entityType)
          cellAvailability(newMapCell).flatMap { availability =>
            if (availability == targetAvailability) {
              Future
                .sequence(Entity.cells(currentMapEntity.size, newCell).map(cellAvailability(grid, _)))
                .map { cellsAvailability =>
                  if (cellsAvailability.forall(_ == targetAvailability)) {
                    val dissocEntities = dissociateMapEntity(
                      grid,
                      entities,
                      currentMapEntity,
                      currentCell
                    )

                    val assocEntities = associateMapEntity(
                      grid,
                      dissocEntities,
                      currentMapEntity.withNewParentCell(newCell),
                      newCell
                    )

                    (assocEntities, Event(Event.System.EntityMoved, Some(newCell)))
                  } else {
                    (entities, Event(Event.System.CellsUnavailable, Some(newCell)))
                  }
                }
            } else {
              Future.successful((entities, Event(Event.System.CellsUnavailable, Some(newCell))))
            }
          }

        case None =>
          Future.successful((entities, Event(Event.System.CellsUnavailable, Some(newCell))))
      }
    }

    result match {
      case Left(event) =>
        tracker ! event
        Future.successful(entities)

      case Right(data) =>
        data.map {
          case (updatedEntities, event) =>
            tracker ! event
            updatedEntities
        }
    }
  }

  def associateMapEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    mapEntity: MapEntity,
    cell: Point
  ): Map[EntityActorRef, Point] = {
    val cells = Entity.cells(mapEntity.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_ ! AddEntity(mapEntity.entityRef, mapEntity))

    mapEntity.entityRef match {
      case _: Doodad | _: Road      => addDesirability(grid, mapEntity.desirability, cells)
      case _: Structure.ActorRefTag => addDesirability(grid, mapEntity.desirability, cells)
      case _                        => () //do nothing
    }

    entities + (mapEntity.entityRef -> cell)
  }

  def dissociateMapEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    mapEntity: MapEntity,
    cell: Point
  ): Map[EntityActorRef, Point] = {
    val cells = Entity.cells(mapEntity.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_ ! RemoveEntity(mapEntity.entityRef))

    mapEntity.entityRef match {
      case _: Doodad | _: Road      => removeDesirability(grid, mapEntity.desirability, cells)
      case _: Structure.ActorRefTag => removeDesirability(grid, mapEntity.desirability, cells)
      case _                        => () //do nothing
    }

    entities - mapEntity.entityRef
  }

  def addDesirability(
    grid: Grid[CellActorRef],
    entityDesirability: EntityDesirability,
    cells: Seq[Point]
  ): Unit =
    applyDesirability(grid, entityDesirability, cells, modifier = 1)

  def removeDesirability(
    grid: Grid[CellActorRef],
    entityDesirability: EntityDesirability,
    cells: Seq[Point]
  ): Unit =
    applyDesirability(grid, entityDesirability, cells, modifier = -1)

  private def applyDesirability(
    grid: Grid[CellActorRef],
    entityDesirability: EntityDesirability,
    cells: Seq[Point],
    modifier: Int
  ): Unit = {
    val _ = entityDesirability.toMap.foldLeft(Seq.empty[Point]) {
      case (processedCells, (radius, desirability)) =>
        val currentWindowCells =
          cells.flatMap(grid.window(_, radius).toMap).filter {
            case (point, _) => !processedCells.contains(point)
          }

        currentWindowCells.foreach {
          case (_, affectedCell) =>
            affectedCell ! UpdateDesirability(desirability * modifier)
        }

        processedCells ++ currentWindowCells.map(_._1)
    }
  }
}
