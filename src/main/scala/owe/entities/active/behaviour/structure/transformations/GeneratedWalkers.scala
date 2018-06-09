package owe.entities.active.behaviour.structure.transformations

import akka.actor.ActorRef
import owe.Tagging.@@
import owe.entities.ActiveEntity.{ActorRefTag, ForwardMessage, StructureData}
import owe.entities.active.Structure.{State, WalkerState, WalkersProperties, WalkersState}
import owe.map.GameMap.CreateEntity

trait GeneratedWalkers {
  private[behaviour] val parentEntity: ActorRef @@ ActorRefTag

  def withGeneratedWalkers(structure: StructureData): State =
    (structure.properties.walkers, structure.state.walkers) match {
      case (WalkersProperties(generators), WalkersState(state)) =>
        generators.foldLeft(structure.state) {
          case (currentState, (walkerName, walkerGenerator)) =>
            walkerGenerator(structure) match {
              case Some(walker) =>
                parentEntity ! ForwardMessage(CreateEntity(walker, structure.properties.homePosition))
                currentState.copy(walkers = WalkersState(state + (walkerName -> WalkerState.Available)))

              case None => currentState //do nothing
            }
        }

      case _ => structure.state //can't generate walkers; data missing
    }
}