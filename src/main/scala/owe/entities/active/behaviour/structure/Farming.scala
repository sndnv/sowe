package owe.entities.active.behaviour.structure

trait Farming extends BaseStructure {
  override protected def behaviour: Behaviour = farming()
}
