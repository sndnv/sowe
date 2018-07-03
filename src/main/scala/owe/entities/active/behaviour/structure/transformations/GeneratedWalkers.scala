package owe.entities.active.behaviour.structure.transformations

import owe.entities.ActiveEntity.{ActiveEntityRef, ForwardMessage, StructureData}
import owe.entities.active.Structure.{State, WalkerState, WalkersProperties, WalkersState}
import owe.map.GameMap.CreateEntity

trait GeneratedWalkers {
  private[behaviour] val parentEntity: ActiveEntityRef

  def withGeneratedWalkers(structure: StructureData): State =
    (structure.properties.walkers, structure.state.walkers) match {
      case (WalkersProperties(generators), WalkersState(state)) =>
        generators.foldLeft(structure.state) {
          case (currentState, (walkerName, walkerGenerator)) =>
            val isWalkerAvailable = state.get(walkerName).contains(WalkerState.Available)

            if (!isWalkerAvailable) {
              walkerGenerator(structure) match {
                case Some(walker) =>
                  parentEntity ! ForwardMessage(CreateEntity(walker, structure.properties.homePosition))
                  currentState.copy(walkers = WalkersState(state + (walkerName -> WalkerState.Available)))

                case None => currentState //do nothing
              }
            } else {
              currentState //walker already exists
            }
        }

      case _ => structure.state //can't generate walkers; data missing
    }
}
