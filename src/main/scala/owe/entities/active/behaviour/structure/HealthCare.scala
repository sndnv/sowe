package owe.entities.active.behaviour.structure

trait HealthCare extends BaseStructure {
  override protected def behaviour: Behaviour = producing()
}
