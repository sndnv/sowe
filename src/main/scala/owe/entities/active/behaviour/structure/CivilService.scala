package owe.entities.active.behaviour.structure

trait CivilService extends BaseStructure {
  override protected def behaviour: Behaviour = producing()
}
