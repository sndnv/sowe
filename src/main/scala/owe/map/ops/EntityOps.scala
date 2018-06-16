package owe.map.ops

import java.util.UUID

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import owe.Tagging._
import owe.entities.active.{Resource, Structure, Walker}
import owe.entities.passive.{Doodad, Road, Roadblock}
import owe.entities.{ActiveEntity, Entity, PassiveEntity}
import owe.events.Event
import owe.map.Cell._
import owe.map._
import owe.map.grid.{Grid, Point}
import owe.{EntityDesirability, EntityID}

import scala.concurrent.{ExecutionContext, Future}

trait EntityOps { _: AvailabilityOps =>

  protected val tracker: ActorRef

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def createEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityID, Point],
    entity: Entity,
    cell: Point,
    actorFactory: Props => ActorRef
  ): Future[Map[EntityID, Point]] =
    grid.get(cell) match {
      case Some(mapCell) =>
        val targetAvailability = requiredAvailability(entity.`type`)
        cellAvailability(mapCell).flatMap { availability =>
          if (availability == targetAvailability) {
            Future
              .sequence(entityCells(entity.`size`, cell).map(cellAvailability(grid, _)))
              .map { cellsAvailability =>
                if (cellsAvailability.forall(_ == targetAvailability)) {
                  val entityID = UUID.randomUUID()
                  val mapEntity: MapEntity = entity match {
                    case entity: ActiveEntity[_, _, _, _, _] =>
                      ActiveMapEntity(
                        actorFactory(entity.props()).tag[entity.Tag],
                        cell,
                        entity.`size`,
                        entity.`desirability`
                      )

                    case entity: PassiveEntity =>
                      PassiveMapEntity(entity, cell, entity.`desirability`)
                  }

                  tracker ! Event(Event.System.EntityCreated, Some(cell))
                  associateMapEntity(grid, entities, mapEntity, entityID, cell)
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
    entities: Map[EntityID, Point],
    entityID: EntityID
  ): Future[Map[EntityID, Point]] =
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
                dissociateMapEntity(grid, entities, mapEntity, entityID, cell)

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
    entities: Map[EntityID, Point],
    entityID: EntityID,
    newCell: Point
  ): Future[Map[EntityID, Point]] = {

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
          val targetAvailability = requiredAvailability(entityTypeFromMapEntity(currentMapEntity))
          cellAvailability(newMapCell).flatMap { availability =>
            if (availability == targetAvailability) {
              Future
                .sequence(entityCells(currentMapEntity.size, newCell).map(cellAvailability(grid, _)))
                .map { cellsAvailability =>
                  if (cellsAvailability.forall(_ == targetAvailability)) {
                    val dissocEntities = dissociateMapEntity(
                      grid,
                      entities,
                      currentMapEntity,
                      entityID,
                      currentCell
                    )

                    val assocEntities = associateMapEntity(
                      grid,
                      dissocEntities,
                      currentMapEntity.withNewParentCell(newCell),
                      entityID,
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

  def entityTypeFromMapEntity(mapEntity: MapEntity): Entity.Type =
    mapEntity match {
      case PassiveMapEntity(entity, _, _) =>
        entity match {
          case _: Doodad    => Entity.Type.Doodad
          case _: Road      => Entity.Type.Road
          case _: Roadblock => Entity.Type.Roadblock
        }

      case ActiveMapEntity(entity, _, _, _) =>
        entity match {
          case _: Resource.ActorRefTag  => Entity.Type.Resource
          case _: Structure.ActorRefTag => Entity.Type.Structure
          case _: Walker.ActorRefTag    => Entity.Type.Walker
        }
    }

  def associateMapEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityID, Point],
    mapEntity: MapEntity,
    entityID: EntityID,
    cell: Point
  ): Map[EntityID, Point] = {
    val cells = entityCells(mapEntity.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_ ! AddEntity(entityID, mapEntity))

    mapEntity match {
      case PassiveMapEntity(entity, _, desirability) =>
        entity match {
          case _: Doodad | _: Road => addDesirability(grid, desirability, cells)
          case _                   => () //do nothing
        }

      case ActiveMapEntity(entity, _, _, desirability) =>
        entity match {
          case _: Structure.ActorRefTag => addDesirability(grid, desirability, cells)
          case _                        => () //do nothing
        }
    }

    entities + (entityID -> cell)
  }

  def dissociateMapEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityID, Point],
    mapEntity: MapEntity,
    entityID: EntityID,
    cell: Point
  ): Map[EntityID, Point] = {
    val cells = entityCells(mapEntity.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_ ! RemoveEntity(entityID))

    mapEntity match {
      case PassiveMapEntity(entity, _, desirability) =>
        entity match {
          case _: Doodad | _: Road => removeDesirability(grid, desirability, cells)
          case _                   => () //do nothing
        }

      case ActiveMapEntity(entity, _, _, desirability) =>
        entity match {
          case _: Structure.ActorRefTag => removeDesirability(grid, desirability, cells)
          case _                        => () //do nothing
        }
    }

    entities - entityID
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
