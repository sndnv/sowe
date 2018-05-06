package owe.entities.active.behaviour.structure

trait Monument extends BaseStructure {
  override protected def behaviour: Behaviour = producing()
}
