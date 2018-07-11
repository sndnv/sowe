package owe.entities.active.behaviour.structure.transformations

import owe.entities.ActiveEntity.StructureData
import owe.entities.active.Structure.{RiskModifier, RiskState, State}
import owe.entities.active.attributes.{Life, RiskAmount}

trait ProcessedRisk {
  def withProcessedRisk(structure: StructureData): State =
    (structure.state.risk, structure.modifiers.risk) match {
      case (state: RiskState, modifiers: RiskModifier) =>
        val updatedFireState = state.fire + modifiers.fire
        val updatedDamageState = state.damage + modifiers.damage

        if (updatedFireState >= RiskAmount.max || updatedDamageState >= RiskAmount.max) {
          structure.state.copy(
            currentLife = Life(0)
          )
        } else {
          structure.state.copy(
            risk = RiskState(
              fire = updatedFireState,
              damage = updatedDamageState
            )
          )
        }

      case _ => structure.state //can't calculate risk; risk data missing
    }
}
