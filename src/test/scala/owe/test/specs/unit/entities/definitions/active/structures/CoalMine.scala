package owe.test.specs.unit.entities.definitions.active.structures

import owe.effects.Effect
import owe.entities.active.Structure
import owe.map.MapCell
import owe.{entities, EffectID, EntityDesirability}

object CoalMine extends Structure {

  override def `size`: entities.Entity.Size = entities.Entity.Size(height = 3, width = 2)

  override def `desirability`: EntityDesirability = EntityDesirability(-5, -5, -3, -3, -3, -3)

  override def `subtype`: Structure.Type = Structure.Type.Industry

  override protected def createProperties(): Structure.Properties = Structure.Properties(
    name = "CoalMine",
    maxPeople = 15,
    cost = 500
  )

  override protected def createState(): Structure.State = Structure.State(
    risk = Some(Structure.Risk(fire = 0, damage = 0)),
    commodities = Some(Structure.Commodities(available = Map.empty)),
    housing = None,
    production = Some(Structure.Production(employees = 0, rate = 0))
  )

  override protected def createStateModifiers(): Structure.StateModifiers = Structure.StateModifiers(
    risk = Some(Structure.RiskModifier(fire = 3, damage = 5)),
    commodities = Some(Structure.CommoditiesModifier(usageRates = Map.empty)),
    housing = None,
    production = Some(Structure.ProductionModifier(rate = 0))
  )

  override protected def createEffects(): Seq[((Structure.Properties, Structure.State) => Boolean, Effect)] = Seq.empty

  override protected def processCommodities(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Option[Structure.Commodities],
    modifiers: Option[Structure.CommoditiesModifier]
  ): Option[Structure.Commodities] = ??? //TODO

  override protected def processProduction(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Option[Structure.Production],
    modifiers: Option[Structure.ProductionModifier]
  ): Option[Structure.Production] = ??? //TODO
}
