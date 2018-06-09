package owe.entities.active.behaviour.walker.transformations

import owe.entities.ActiveEntity.WalkerData
import owe.entities.active.Walker.State
import owe.map.grid.Point

import scala.collection.immutable.Queue
import scala.concurrent.Future

trait ProcessedPath {
  def withProcessedPath(walker: WalkerData, updatedPath: Queue[Point]): Future[State] =
    Future.successful(
      walker.state.copy(
        path = updatedPath
      )
    )
}
