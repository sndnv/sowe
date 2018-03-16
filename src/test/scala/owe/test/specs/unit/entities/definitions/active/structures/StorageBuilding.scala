package owe.test.specs.unit.entities.definitions.active.structures

import owe.effects.Effect
import owe.entities.active.Structure
import owe.map.MapCell
import owe.{entities, EffectID, EntityDesirability}

object StorageBuilding extends Structure {

  override def `desirability`: EntityDesirability = EntityDesirability(-5, -5, -3, -3, -1, -1)

  override def `subtype`: Structure.Type = Structure.Type.Industry

  override protected def createProperties(): Structure.Properties = Structure.Properties(
    name = "StorageBuilding",
    maxPeople = 10,
    cost = 150
  )

  override protected def createState(): Structure.State = Structure.State(
    risk = Some(Structure.Risk(fire = 0, damage = 0)),
    commodities = Some(Structure.Commodities(available = Map.empty)),
    housing = None,
    production = None
  )

  override protected def createStateModifiers(): Structure.StateModifiers = Structure.StateModifiers(
    risk = Some(Structure.RiskModifier(fire = 2, damage = 1)),
    commodities = Some(Structure.CommoditiesModifier(usageRates = Map.empty)),
    housing = None,
    production = None
  )

  override protected def createEffects(): Seq[((Structure.Properties, Structure.State) => Boolean, Effect)] = Seq.empty

  override def `size`: entities.Entity.Size = entities.Entity.Size(height = 4, width = 4)

  override protected def processCommodities(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Option[Structure.Commodities],
    modifiers: Option[Structure.CommoditiesModifier]
  ): Option[Structure.Commodities] = ??? //TODO
}
