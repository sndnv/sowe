package owe.entities.active.behaviour.structure

trait Education extends BaseStructure {
  override protected def behaviour: Behaviour = producing()
}
