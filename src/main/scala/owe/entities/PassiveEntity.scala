package owe.entities

trait PassiveEntity extends Entity {
  override def `size`: Entity.Size = Entity.Size(height = 1, width = 1)
}
