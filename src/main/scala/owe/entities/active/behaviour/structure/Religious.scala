package owe.entities.active.behaviour.structure

trait Religious extends BaseStructure {
  override protected def behaviour: Behaviour = producing()
}
