package owe.entities.active.behaviour.walker.transformations

import owe.entities.ActiveEntity.WalkerData
import owe.entities.active.Walker.{MovementMode, State}
import owe.entities.active.attributes.Distance

import scala.collection.immutable.Queue
import scala.concurrent.Future

trait ProcessedMovement {
  def withProcessedMovement(walker: WalkerData): Future[State] =
    Future.successful(
      walker.state.copy(
        distanceCovered = walker.state.distanceCovered + Distance(1)
      )
    )

  def withMovementMode(walker: WalkerData, newMode: MovementMode): Future[State] =
    Future.successful(
      walker.state.copy(
        mode = newMode
      )
    )
}
