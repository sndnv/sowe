package owe.entities.active.behaviour.walker.idling

import owe.entities.active.behaviour.walker.BaseWalker

trait Military extends BaseWalker {
  final override protected def behaviour: Behaviour = idling()
}
