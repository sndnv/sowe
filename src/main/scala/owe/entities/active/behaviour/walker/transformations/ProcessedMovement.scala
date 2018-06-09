package owe.entities.active.behaviour.walker.transformations

import owe.entities.ActiveEntity.WalkerData
import owe.entities.active.Distance
import owe.entities.active.Walker.{MovementMode, State}

import scala.collection.immutable.Queue
import scala.concurrent.Future

trait ProcessedMovement {
  def withProcessedMovement(walker: WalkerData): Future[State] =
    Future.successful(
      walker.state.copy(
        distanceCovered = walker.state.distanceCovered + Distance(1),
        destinationPath = Queue.empty
      )
    )

  def withMovementMode(walker: WalkerData, newMode: MovementMode): Future[State] =
    Future.successful(
      walker.state.copy(
        mode = newMode
      )
    )
}
