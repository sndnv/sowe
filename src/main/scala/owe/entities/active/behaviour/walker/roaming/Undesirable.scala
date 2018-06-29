package owe.entities.active.behaviour.walker.roaming

import scala.concurrent.ExecutionContext

import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker.NoAction

trait Undesirable extends BaseWalker {
  final override protected def behaviour(implicit ec: ExecutionContext): Behaviour = roaming(NoAction)
}
