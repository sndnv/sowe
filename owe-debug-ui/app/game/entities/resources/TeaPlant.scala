package game.entities.resources

import owe.effects.Effect
import owe.entities.ActiveEntity.{ActiveEntityRef, Data, ResourceData}
import owe.entities.active.Resource
import owe.entities.active.Resource.{Properties, ResourceRef, State, StateModifiers}
import owe.entities.active.behaviour.resource.producing.ProducingResource
import owe.map.grid.Point
import owe.production.Commodity

class TeaPlant(home: Point) extends Resource {
  override protected def createActiveEntityData(): ActiveEntityRef => Data = {
    case id: ResourceRef =>
      ResourceData(
        properties = Properties(
          name = "TeaPlant",
          homePosition = home,
          commodity = Commodity("Tea"),
          maxAmount = Commodity.Amount(500)
        ),
        state = State(
          currentAmount = Commodity.Amount(0),
          replenishAmount = Commodity.Amount(5)
        ),
        modifiers = StateModifiers(
          replenishAmount = Commodity.AmountModifier(100)
        ),
        id
      )
  }

  override protected def createBehaviour(): ProducingResource = new ProducingResource {}

  override protected def createEffects(): Seq[(Data => Boolean, Effect)] = Seq.empty
}
