package owe.entities.active.behaviour.walker.transformations

import owe.entities.ActiveEntity.WalkerData
import owe.entities.active.Walker.State
import owe.entities.active.behaviour.walker.BaseWalker.{Action, DoOperation, DoRepeatableOperation}

import scala.concurrent.Future

trait RoamAction {
  def withRoamAction(walker: WalkerData, roamAction: Action): Future[State] =
    roamAction match {
      case DoOperation(op)              => op(walker)
      case DoRepeatableOperation(op, _) => op(walker)
      case _                            => Future.successful(walker.state)
    }
}
