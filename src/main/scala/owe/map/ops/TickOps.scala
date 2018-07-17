package owe.map.ops

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, MapData}
import owe.entities.ActiveEntityActor.{ApplyEffects, GetActiveEffects, ProcessEntityTick}
import owe.entities.PassiveEntity.PassiveEntityRef
import owe.map.Cell
import owe.map.Cell.{CellActorRef, CellData, GetCellData}
import owe.map.GameMap.TickProcessed
import owe.map.grid.{Grid, Point}

import scala.concurrent.{ExecutionContext, Future}

trait TickOps {

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def processTick(
    grid: Grid[CellActorRef],
    tick: Int,
    start: Point,
    end: Point
  )(implicit sender: ActorRef = Actor.noSender): Future[TickProcessed] =
    for {
      activeEffects <- gatherActiveEffects(grid)
      processedEntities <- processCells(grid, tick, activeEffects, start, end)
    } yield {
      TickProcessed(processedEntities)
    }

  //doc -> a map of points -> effects to be applied to those points
  def gatherActiveEffects(
    grid: Grid[CellActorRef]
  )(implicit sender: ActorRef = Actor.noSender): Future[Map[Point, Seq[Effect]]] = {
    val indexedGrid = grid.indexes()
    Future
      .traverse(
        grid.toMap.mapValues { mapCell =>
          (mapCell ? GetCellData()).mapTo[CellData].flatMap { cellData =>
            Future
              .sequence(
                cellData.entities.map {
                  case (_, mapEntity) =>
                    mapEntity.entityRef match {
                      case entity: ActiveEntityRef =>
                        (entity ? GetActiveEffects()).mapTo[Seq[Effect]]

                      case _: PassiveEntityRef =>
                        Future.successful(Seq.empty)
                    }
                }
              )
              .map(_.flatten)
          }
        }
      ) {
        case (cell, future) =>
          future.map { effectsFromCell =>
            effectsFromCell
              .map(effect => (effect, indexedGrid.window(cell, effect.radius).toSeq))
          }
      }
      .map { effectsOnCells =>
        effectsOnCells.flatten.foldLeft(Map.empty[Point, Seq[Effect]]) {
          case (map, (effect, points)) =>
            val updates = points.map { point =>
              (point, map.getOrElse(point, Seq.empty) :+ effect)
            }

            map ++ updates
        }
      }
  }

  //doc -> end is inclusive
  def processCells(
    grid: Grid[CellActorRef],
    tick: Int,
    activeEffects: Map[Point, Seq[Effect]],
    start: Point,
    end: Point
  )(implicit sender: ActorRef = Actor.noSender): Future[Int] = {
    def processCell(currentCell: Point, processedEntities: Int): Future[Int] =
      grid
        .get(currentCell) match {
        case Some(cell) =>
          (cell ? GetCellData())
            .mapTo[CellData]
            .flatMap { cellData =>
              val updatedCellState = activeEffects
                .get(currentCell)
                .map { effects =>
                  val (cellEffects, entityEffects) =
                    effects.foldLeft((Seq.empty[Cell.Effect], Seq.empty[Effect])) {
                      case ((ce, ee), effect) =>
                        effect match {
                          case effect: Cell.Effect => (ce :+ effect, ee)
                          case _                   => (ce, ee :+ effect)
                        }
                    }

                  cellData.entities.foreach {
                    case (_, mapEntity) =>
                      mapEntity.entityRef match {
                        case entity: ActiveEntityRef =>
                          entity ! ApplyEffects(entityEffects)

                        case _ => () //do nothing
                      }
                  }

                  cellEffects.foldLeft(cellData.state) {
                    case (state, effect) => effect(state)
                  }
                }
                .getOrElse(cellData.state)

              cellData.entities.foreach {
                case (_, mapEntity) =>
                  mapEntity.entityRef match {
                    case entity: ActiveEntityRef =>
                      entity ! ProcessEntityTick(tick, MapData(mapEntity.parentCell, updatedCellState))

                    case _ => () //do nothing
                  }
              }

              if (currentCell == end) {
                Future.successful(processedEntities + cellData.entities.size)
              } else {
                grid
                  .nextPoint(currentCell)
                  .map { nextCell =>
                    processCell(nextCell, processedEntities + cellData.entities.size)
                  }
                  .getOrElse(
                    Future.failed(
                      new IllegalStateException(s"Failed to find next cell after [$currentCell]")
                    )
                  )
              }
            }

        case None =>
          Future.failed(new IllegalArgumentException(s"Failed to find cell at [$currentCell]"))
      }

    processCell(currentCell = start, processedEntities = 0)
  }
}
