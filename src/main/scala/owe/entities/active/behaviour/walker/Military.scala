package owe.entities.active.behaviour.walker

trait Military extends BaseWalker {
  final override protected def behaviour: Behaviour = idling()
}
