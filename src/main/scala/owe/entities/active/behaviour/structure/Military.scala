package owe.entities.active.behaviour.structure

trait Military extends BaseStructure {
  override protected def behaviour: Behaviour = producing()
}
