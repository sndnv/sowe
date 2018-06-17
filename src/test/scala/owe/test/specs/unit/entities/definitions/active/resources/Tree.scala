package owe.test.specs.unit.entities.definitions.active.resources

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityData, ResourceData}
import owe.entities.active.Resource
import owe.entities.active.Resource.{ActiveEntityActorRef, Properties, State, StateModifiers}
import owe.entities.active.behaviour.resource.producing.ProducingResource
import owe.map.grid.Point
import owe.production.{Commodity, CommodityAmount, CommodityAmountModifier}

class Tree extends Resource {
  override protected def createActiveEntityData(): ActiveEntityActorRef => ActiveEntityData = { id =>
    ResourceData(
      properties = Properties(
        name = "Tree",
        homePosition = Point(0, 0),
        commodity = Commodity("Wood"),
        maxAmount = CommodityAmount(500)
      ),
      state = State(
        currentAmount = CommodityAmount(0),
        replenishAmount = CommodityAmount(25)
      ),
      modifiers = StateModifiers(
        replenishAmount = CommodityAmountModifier(100)
      ),
      id
    )
  }

  override protected def createBehaviour(): ProducingResource = new ProducingResource {}

  override protected def createEffects(): Seq[(ActiveEntityData => Boolean, Effect)] = Seq.empty
}
