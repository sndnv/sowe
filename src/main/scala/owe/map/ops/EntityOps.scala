package owe.map.ops

import akka.actor.{Actor, ActorRef}
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
    entity: MapEntity,
    cell: Point
  )(implicit sender: ActorRef = Actor.noSender): Future[Either[Event, Event]] =
    grid.get(cell) match {
      case Some(mapCell) =>
        val targetAvailability = requiredAvailability(entity.entityType)
        cellAvailability(mapCell).flatMap { availability =>
          if (availability >= targetAvailability) {
            Future
              .sequence(Entity.cells(entity.`size`, cell).map(cellAvailabilityForPoint(grid, _)))
              .map { cellsAvailability =>
                if (cellsAvailability.forall(_ >= targetAvailability)) {
                  Right(Event(Event.System.EntityCreated, Some(cell)))
                } else {
                  Left(Event(Event.System.CellsUnavailable, Some(cell)))
                }
              }
          } else {
            Future.successful(Left(Event(Event.System.CellsUnavailable, Some(cell))))
          }
        }

      case None =>
        Future.successful(Left(Event(Event.System.CellOutOfBounds, Some(cell))))
    }

  def destroyEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef
  )(implicit sender: ActorRef = Actor.noSender): Future[Either[Event, (Event, MapEntity, Point)]] =
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
                Right(
                  (
                    Event(Event.System.EntityDestroyed, Some(cell)),
                    mapEntity,
                    cell
                  )
                )

              case None =>
                Left(Event(Event.System.EntityMissing, Some(cell)))
            }

          case _ =>
            Future.successful(Left(Event(Event.System.EntityMissing, Some(cell))))
        }

      case None =>
        Future.successful(Left(Event(Event.System.CellOutOfBounds, cell = None)))
    }

  def moveEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef,
    newCell: Point
  )(implicit sender: ActorRef = Actor.noSender): Future[Either[Event, (Event, MapEntity, Point)]] = {
    val result = for {
      currentCell <- entities
        .get(entityID)
        .toRight(Event(Event.System.CellsUnavailable, cell = None)): Either[Event, Point]
      currentMapCell <- grid
        .get(currentCell)
        .toRight(Event(Event.System.CellOutOfBounds, Some(currentCell))): Either[Event, CellActorRef]
      newMapCell <- grid
        .get(newCell)
        .toRight(Event(Event.System.CellOutOfBounds, Some(newCell))): Either[Event, CellActorRef]
    } yield {
      (currentMapCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].flatMap {
        case Some(currentMapEntity) =>
          val targetAvailability = requiredAvailability(currentMapEntity.entityType)
          cellAvailability(newMapCell).flatMap { availability =>
            if (availability >= targetAvailability) {
              Future
                .sequence(Entity.cells(currentMapEntity.size, newCell).map(cellAvailabilityForPoint(grid, _)))
                .map { cellsAvailability =>
                  if (cellsAvailability.forall(_ >= targetAvailability)) {
                    Right(
                      (
                        Event(Event.System.EntityMoved, Some(newCell)),
                        currentMapEntity,
                        currentCell
                      )
                    )
                  } else {
                    Left(Event(Event.System.CellsUnavailable, Some(newCell)))
                  }
                }
            } else {
              Future.successful(Left(Event(Event.System.CellsUnavailable, Some(newCell))))
            }
          }

        case None =>
          Future.successful(Left(Event(Event.System.CellsUnavailable, Some(newCell))))
      }
    }

    result.left
      .map(Future.successful)
      .fold(_.map(Left(_)), _.map(Right(_)))
      .map(_.joinRight)
  }

  def associateMapEntity(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    mapEntity: MapEntity,
    cell: Point
  )(implicit sender: ActorRef = Actor.noSender): Map[EntityRef, Point] = {
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
  )(implicit sender: ActorRef = Actor.noSender): Map[EntityRef, Point] = {
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
    desirability: Desirability,
    cells: Seq[Point]
  )(implicit sender: ActorRef = Actor.noSender): Unit =
    applyDesirability(grid, desirability, cells, modifier = 1)

  def removeDesirability(
    grid: Grid[CellActorRef],
    desirability: Desirability,
    cells: Seq[Point]
  )(implicit sender: ActorRef = Actor.noSender): Unit =
    applyDesirability(grid, desirability, cells, modifier = -1)

  private def applyDesirability(
    grid: Grid[CellActorRef],
    desirability: Desirability,
    cells: Seq[Point],
    modifier: Int
  )(implicit sender: ActorRef = Actor.noSender): Unit = {
    val _ = desirability.toMap.foldLeft(Seq.empty[Point]) {
      case (processedCells, (radius, currentDesirability)) =>
        val currentWindowCells =
          cells.flatMap(grid.window(_, radius).toMap).filter {
            case (point, _) => !processedCells.contains(point)
          }

        currentWindowCells.foreach {
          case (_, affectedCell) =>
            affectedCell ! UpdateDesirability(currentDesirability * modifier)
        }

        processedCells ++ currentWindowCells.map(_._1)
    }
  }
}
