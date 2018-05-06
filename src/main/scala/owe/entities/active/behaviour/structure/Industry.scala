package owe.entities.active.behaviour.structure

trait Industry extends BaseStructure {
  override protected def behaviour: Behaviour = producing()
}
