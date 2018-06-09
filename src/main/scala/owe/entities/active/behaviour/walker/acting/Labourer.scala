package owe.entities.active.behaviour.walker.acting

import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker.Action

trait Labourer extends BaseWalker {
  protected def actions: Seq[Action]

  override protected def behaviour: Behaviour = acting(actions)
}
