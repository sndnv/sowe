package owe.entities.active.behaviour.walker

import owe.entities.active.behaviour.walker.BaseWalker.NoAction

trait Hero extends BaseWalker {
  final override protected def behaviour: Behaviour = roaming(NoAction)
}
