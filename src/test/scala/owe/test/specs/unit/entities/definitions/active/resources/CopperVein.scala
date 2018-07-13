package owe.test.specs.unit.entities.definitions.active.resources

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, ResourceData}
import owe.entities.active.Resource
import owe.entities.active.Resource.{Properties, ResourceRef, State, StateModifiers}
import owe.entities.active.behaviour.resource.producing.ProducingResource
import owe.map.grid.Point
import owe.production.Commodity

class CopperVein extends Resource {
  override protected def createActiveEntityData(): ActiveEntityRef => Data = {
    case id: ResourceRef =>
      ResourceData(
        properties = Properties(
          name = "CopperVein",
          homePosition = Point(0, 0),
          commodity = Commodity("Copper"),
          maxAmount = Commodity.Amount(50)
        ),
        state = State(
          currentAmount = Commodity.Amount(20),
          replenishAmount = Commodity.Amount(5)
        ),
        modifiers = StateModifiers(
          replenishAmount = Commodity.AmountModifier(75)
        ),
        id
      )
  }

  override protected def createBehaviour(): ProducingResource = new ProducingResource {}

  override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
}
