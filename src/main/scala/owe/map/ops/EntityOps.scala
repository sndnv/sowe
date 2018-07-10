package owe.map.ops

import akka.pattern.ask
import akka.util.Timeout
import owe.entities.Entity
import owe.entities.Entity.{Desirability, EntityRef}
import owe.entities.active.Structure.StructureRef
import owe.entities.passive.Doodad.DoodadRef
import owe.entities.passive.Road.RoadRef
import owe.events.Event
import owe.map.Cell._
import owe.map._
import owe.map.grid.{Grid, Point}

import scala.concurrent.{ExecutionContext, Future}

trait EntityOps { _: AvailabilityOps =>

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def createEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entity: MapEntity,
    cell: Point
  ): Future[(Map[EntityRef, Point], Event)] =
    grid.get(cell) match {
      case Some(mapCell) =>
        val targetAvailability = requiredAvailability(entity.entityType)
        cellAvailability(mapCell).flatMap { availability =>
          if (availability == targetAvailability) {
            Future
              .sequence(Entity.cells(entity.`size`, cell).map(cellAvailability(grid, _)))
              .map { cellsAvailability =>
                if (cellsAvailability.forall(_ == targetAvailability)) {
                  val event = Event(Event.System.EntityCreated, Some(cell))
                  (associateMapEntity(grid, entities, entity, cell), event)
                } else {
                  val event = Event(Event.System.CellsUnavailable, Some(cell))
                  (entities, event)
                }
              }
          } else {
            val event = Event(Event.System.CellsUnavailable, Some(cell))
            Future.successful((entities, event))
          }
        }

      case None =>
        val event = Event(Event.System.CellOutOfBounds, Some(cell))
        Future.successful((entities, event))
    }

  def destroyEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef
  ): Future[(Map[EntityRef, Point], Event)] =
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
                val event = Event(Event.System.EntityDestroyed, Some(cell))
                (dissociateMapEntity(grid, entities, mapEntity, cell), event)

              case None =>
                val event = Event(Event.System.EntityMissing, Some(cell))
                (entities, event)
            }

          case _ =>
            val event = Event(Event.System.EntityMissing, Some(cell))
            Future.successful((entities, event))
        }

      case None =>
        val event = Event(Event.System.CellOutOfBounds, cell = None)
        Future.successful((entities, event))
    }

  def moveEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef,
    newCell: Point
  ): Future[(Map[EntityRef, Point], Event)] = {

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
                      currentMapEntity.copy(parentCell = newCell),
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
      case Left(event) => Future.successful((entities, event))
      case Right(data) => data
    }
  }

  def associateMapEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    mapEntity: MapEntity,
    cell: Point
  ): Map[EntityRef, Point] = {
    val cells = Entity.cells(mapEntity.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_ ! AddEntity(mapEntity))

    mapEntity.entityRef match {
      case _: DoodadRef | _: RoadRef | _: StructureRef =>
        addDesirability(grid, mapEntity.desirability, cells)

      case _ =>
        () //do nothing
    }

    entities + (mapEntity.entityRef -> cell)
  }

  def dissociateMapEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    mapEntity: MapEntity,
    cell: Point
  ): Map[EntityRef, Point] = {
    val cells = Entity.cells(mapEntity.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_ ! RemoveEntity(mapEntity.entityRef))

    mapEntity.entityRef match {
      case _: DoodadRef | _: RoadRef | _: StructureRef =>
        removeDesirability(grid, mapEntity.desirability, cells)

      case _ =>
        () //do nothing
    }

    entities - mapEntity.entityRef
  }

  def addDesirability(
    grid: Grid[CellActorRef],
    Desirability: Desirability,
    cells: Seq[Point]
  ): Unit =
    applyDesirability(grid, Desirability, cells, modifier = 1)

  def removeDesirability(
    grid: Grid[CellActorRef],
    Desirability: Desirability,
    cells: Seq[Point]
  ): Unit =
    applyDesirability(grid, Desirability, cells, modifier = -1)

  private def applyDesirability(
    grid: Grid[CellActorRef],
    Desirability: Desirability,
    cells: Seq[Point],
    modifier: Int
  ): Unit = {
    val _ = Desirability.toMap.foldLeft(Seq.empty[Point]) {
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
