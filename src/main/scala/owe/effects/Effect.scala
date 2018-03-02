package owe.effects

trait Effect[P, S, M] {
  def apply(properties: P, state: S, modifiers: M): M
}
