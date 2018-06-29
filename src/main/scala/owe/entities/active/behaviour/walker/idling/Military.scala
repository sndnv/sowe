package owe.entities.active.behaviour.walker.idling

import scala.concurrent.ExecutionContext

import owe.entities.active.behaviour.walker.BaseWalker

trait Military extends BaseWalker {
  final override protected def behaviour(implicit ec: ExecutionContext): Behaviour = idling()
}
