package owe.entities.active.behaviour.walker.roaming

import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker.NoAction

trait ReligiousOfficial extends BaseWalker {
  final override protected def behaviour: Behaviour = roaming(NoAction)
}
