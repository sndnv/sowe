package owe.map.ops

import owe.EntityID
import owe.entities.Entity
import owe.entities.active.{Resource, Structure, Walker}
import owe.entities.passive.{Doodad, Road, Roadblock}
import owe.map.MapCell.Availability
import owe.map.grid.{Grid, Point}
import owe.map.{ActiveMapEntity, MapCell, PassiveMapEntity}

trait AvailabilityOps {
  def cellAvailability(cell: MapCell): Availability =
    cell.entities
      .find {
        case (_, mapEntity) =>
          mapEntity match {
            case PassiveMapEntity(entity, _, _) =>
              entity match {
                case _: Doodad    => true
                case _: Road      => false
                case _: Roadblock => false
              }

            case ActiveMapEntity(entity, _, _, _) =>
              entity match {
                case _: Structure.ActorRefTag => true
                case _: Resource.ActorRefTag  => true
                case _: Walker.ActorRefTag    => false
              }
          }
      } match {
      case Some(_) =>
        Availability.Occupied

      case None =>
        if (cell.entities.isEmpty) {
          Availability.Buildable
        } else {
          Availability.Passable
        }
    }

  def cellAvailability(grid: Grid[MapCell], cell: Point): Availability =
    grid
      .get(cell)
      .map(cellAvailability)
      .getOrElse(Availability.OutOfBounds)

  def cellHasRoad(mapCell: MapCell): Boolean =
    mapCell.entities.exists {
      case (_, entity) =>
        entity match {
          case PassiveMapEntity(passiveEntity, _, _) =>
            passiveEntity match {
              case _: Road => true
              case _       => false
            }

          case _ => false
        }
    }

  def requiredAvailability(entityType: Entity.Type): Availability = entityType match {
    case Entity.Type.Doodad    => Availability.Buildable
    case Entity.Type.Road      => Availability.Buildable
    case Entity.Type.Roadblock => Availability.Passable //TODO - can be built only on roads w/o walkers
    case Entity.Type.Resource  => Availability.Buildable
    case Entity.Type.Structure => Availability.Buildable
    case Entity.Type.Walker    => Availability.Passable
  }

  def findFirstAdjacentRoad(grid: Grid[MapCell], entities: Map[EntityID, Point], entityID: EntityID): Option[Point] = {
    for {
      parentCell <- entities.get(entityID)
      mapCell <- grid.get(parentCell)
      mapEntity <- mapCell.entities.get(entityID)
    } yield {
      val cells = entityCells(mapEntity.size, parentCell)
      cells
        .flatMap(point => grid.indexes().window(point, radius = 1).toSeq)
        .distinct
        .flatMap(point => grid.get(point).map(cell => (point, cell)))
        .collect {
          case (point, cell) if !cells.contains(point) && cellHasRoad(cell) => point
        }
        .sorted
        .headOption
    }
  }.flatten

  def entityCells(entitySize: Entity.Size, parentCell: Point): Seq[Point] =
    (parentCell.x to entitySize.width)
      .flatMap(
        x => (parentCell.y to entitySize.height).map(y => Point(x, y))
      )
}
