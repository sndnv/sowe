package owe.map.ops

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import owe.entities.ActiveEntityActor.DestroySelf
import owe.entities.Entity
import owe.entities.Entity.{Desirability, EntityRef}
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker
import owe.entities.active.Walker.SpawnLocation
import owe.entities.passive.Doodad.DoodadRef
import owe.entities.passive.Road.RoadRef
import owe.events.Event
import owe.events.Event.{CellEvent, EntityEvent, SystemEvent}
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
    entity: Entity,
    actorRef: ActorRef,
    defaultCell: Point
  )(implicit sender: ActorRef = Actor.noSender): Future[Either[Event, (MapEntity, Event)]] = {
    val spawnPoint = entity match {
      case walker: Walker =>
        walker.spawnLocation match {
          case SpawnLocation.AdjacentPoint(entityID) =>
            findFirstAdjacentPoint(grid, entities, entityID, _.isPassable)

          case SpawnLocation.AdjacentRoad(entityID) =>
            findFirstAdjacentPoint(grid, entities, entityID, _.hasRoad)

          case SpawnLocation.AtPoint =>
            Future.successful(Some(defaultCell))
        }
      case _ => Future.successful(Some(defaultCell))
    }

    spawnPoint
      .flatMap {
        case Some(point) =>
          val entityCells = Entity.cells(entity.`size`, point)
          if (entityCells.forall(grid.hasPoint)) {
            val mapEntity = MapEntity(actorRef, point, entity)

            Future
              .sequence(entityCells.flatMap(cellAvailability(grid, _)))
              .map { cellsAvailability =>
                if (cellsAvailability.forall(mapEntity.spec.acceptsAvailability)) {
                  Right((mapEntity, EntityEvent(Event.Engine.EntityCreated, mapEntity, point)))
                } else {
                  Left(CellEvent(Event.Engine.CellsUnavailable, point))
                }
              }
          } else {
            Future.successful(Left(CellEvent(Event.Engine.CellOutOfBounds, point)))
          }

        case None =>
          Future.successful(
            Left(SystemEvent(Event.Engine.SpawnPointUnavailable))
          )
      }
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
          case availability if availability.entityTypes.nonEmpty =>
            (mapCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].map {
              case Some(mapEntity) =>
                mapEntity.entityRef ! DestroySelf()

                Right(
                  (
                    EntityEvent(Event.Engine.EntityDestroyed, mapEntity, cell),
                    mapEntity,
                    cell
                  )
                )

              case None =>
                Left(CellEvent(Event.Engine.EntityMissing, cell))
            }

          case _ =>
            Future.successful(Left(CellEvent(Event.Engine.EntityMissing, cell)))
        }

      case None =>
        Future.successful(Left(SystemEvent(Event.Engine.CellOutOfBounds)))
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
        .toRight(SystemEvent(Event.Engine.CellsUnavailable)): Either[Event, Point]
      currentMapCell <- grid
        .get(currentCell)
        .toRight(CellEvent(Event.Engine.CellOutOfBounds, currentCell)): Either[Event, CellActorRef]
      _ <- grid
        .get(newCell)
        .toRight(CellEvent(Event.Engine.CellOutOfBounds, newCell)): Either[Event, CellActorRef]
    } yield {
      (currentMapCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].flatMap {
        case Some(currentMapEntity) =>
          val entityCells = Entity.cells(currentMapEntity.spec.`size`, newCell)
          if (entityCells.forall(grid.hasPoint)) {
            Future
              .sequence(Entity.cells(currentMapEntity.spec.`size`, newCell).flatMap(cellAvailability(grid, _)))
              .map { cellsAvailability =>
                if (cellsAvailability.forall(currentMapEntity.spec.acceptsAvailability)) {
                  Right(
                    (
                      EntityEvent(Event.Engine.EntityMoved, currentMapEntity, newCell),
                      currentMapEntity,
                      currentCell
                    )
                  )
                } else {
                  Left(CellEvent(Event.Engine.CellsUnavailable, newCell))
                }
              }
          } else {
            Future.successful(Left(CellEvent(Event.Engine.CellsUnavailable, newCell)))
          }

        case None =>
          Future.successful(Left(CellEvent(Event.Engine.CellsUnavailable, newCell)))
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
    val cells = Entity.cells(mapEntity.spec.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_ ! AddEntity(mapEntity))

    mapEntity.entityRef match {
      case _: DoodadRef | _: RoadRef | _: StructureRef =>
        addDesirability(grid, mapEntity.spec.desirability, cells)

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
    val cells = Entity.cells(mapEntity.spec.size, mapEntity.parentCell)
    cells.flatMap(grid.get).foreach(_ ! RemoveEntity(mapEntity.entityRef))

    mapEntity.entityRef match {
      case _: DoodadRef | _: RoadRef | _: StructureRef =>
        removeDesirability(grid, mapEntity.spec.desirability, cells)

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
