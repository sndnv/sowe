package owe.entities.active.behaviour.structure

trait Entertainment extends BaseStructure {
  override protected def behaviour: Behaviour = producing()
}
