package owe.map.ops

import akka.pattern.ask
import akka.util.Timeout
import owe.effects.Effect
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.MapData
import owe.map.GameMap.TickProcessed
import owe.map.grid.{Grid, Point}
import owe.map.{ActiveMapEntity, MapCell, PassiveMapEntity}

import scala.concurrent.{ExecutionContext, Future}

trait TickOps {

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def processTick(grid: Grid[MapCell], start: Point, end: Point): Future[TickProcessed] =
    for {
      activeEffects <- gatherActiveEffects(grid)
      processedCells <- processCells(grid, activeEffects, start, end)
    } yield {
      TickProcessed(processedCells)
    }

  //doc -> a map of points -> effects to be applied to those points
  private def gatherActiveEffects(grid: Grid[MapCell]): Future[Map[Point, Seq[Effect]]] = {
    val indexedGrid = grid.indexes()
    Future
      .traverse(
        grid.toMap.mapValues { mapCell =>
          Future
            .sequence(
              mapCell.entities.map {
                case (_, mapEntity) =>
                  mapEntity match {
                    case ActiveMapEntity(entity, _, _, _) =>
                      (entity ? ActiveEntity.GetActiveEffects()).mapTo[Seq[Effect]]

                    case _: PassiveMapEntity =>
                      Future.successful(Seq.empty)
                  }
              }
            )
            .map(_.flatten)
        }
      ) {
        case (point, future) =>
          future.map { result =>
            result.map(effect => (effect, indexedGrid.window(point, effect.radius).toSeq))
          }
      }
      .map { result =>
        result.flatten.foldLeft(Map.empty[Point, Seq[Effect]]) {
          case (map, (effect, points)) =>
            val updates = points.map { point =>
              (point, map.getOrElse(point, Seq.empty) :+ effect)
            }

            map ++ updates
        }
      }
  }

  //doc -> end is inclusive
  private def processCells(
    grid: Grid[MapCell],
    activeEffects: Map[Point, Seq[Effect]],
    start: Point,
    end: Point
  ): Future[Int] = {
    def processCell(currentCell: Point, processedCells: Int): Future[Int] =
      grid
        .get(currentCell)
        .flatMap { cell =>
          activeEffects.get(currentCell).map { effects =>
            val (cellEffects, entityEffects) =
              effects.foldLeft((Seq.empty[MapCell.Effect], Seq.empty[Effect])) {
                case ((ce, ee), effect) =>
                  effect match {
                    case effect: MapCell.Effect => (ce :+ effect, ee)
                    case _                      => (ce, ee :+ effect)
                  }
              }

            val cellModifiers = cellEffects.foldLeft(cell.modifiers) {
              case (modifiers, effect) =>
                effect(cell.properties, modifiers)
            }

            cell.entities.foreach {
              case (_, mapEntity) =>
                mapEntity match {
                  case ActiveMapEntity(entity, _, _, _) =>
                    entity ! ActiveEntity.ApplyEffects(entityEffects)
                    entity ! ActiveEntity.ProcessGameTick(MapData(mapEntity.parentCell, cell.properties, cellModifiers))

                  case _ => () //do nothing
                }
            }

            grid
              .nextPoint(currentCell)
              .map { nextCell =>
                if (nextCell == end) {
                  Future.successful(processedCells + 1)
                } else {
                  processCell(nextCell, processedCells + 1)
                }
              }
              .getOrElse(Future.failed(new IllegalStateException(s"Failed to find next cell after [$currentCell]")))
          }
        }
        .getOrElse(Future.failed(new IllegalArgumentException(s"Failed to find cell at [$currentCell]")))

    processCell(currentCell = start, processedCells = 0)
  }
}