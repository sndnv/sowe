package owe.entities.active.behaviour.walker.roaming

import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker.NoAction

trait Resident extends BaseWalker {
  override protected def behaviour: Behaviour = roaming(NoAction)
}
