package owe.test.specs.unit.entities.definitions.active.resources

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, ActiveEntityRef, ResourceData}
import owe.entities.active.Resource
import owe.entities.active.Resource.{Properties, ResourceRef, State, StateModifiers}
import owe.entities.active.behaviour.resource.producing.ProducingResource
import owe.map.grid.Point
import owe.production.Commodity

class Tree extends Resource {
  override protected def createActiveEntityData(): ActiveEntityRef => ActiveEntityData = {
    case id: ResourceRef =>
      ResourceData(
        properties = Properties(
          name = "Tree",
          homePosition = Point(0, 0),
          commodity = Commodity("Wood"),
          maxAmount = Commodity.Amount(500)
        ),
        state = State(
          currentAmount = Commodity.Amount(0),
          replenishAmount = Commodity.Amount(25)
        ),
        modifiers = StateModifiers(
          replenishAmount = Commodity.AmountModifier(100)
        ),
        id
      )
  }

  override protected def createBehaviour(): ProducingResource = new ProducingResource {}

  override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = Seq.empty
}
