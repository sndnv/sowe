package owe.entities.active.behaviour.structure

trait Housing extends BaseStructure {
  override protected def behaviour: Behaviour = housing()
}
