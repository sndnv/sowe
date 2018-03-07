package owe.test.specs.unit.entities.definitions.active.structures

import owe.effects.Effect
import owe.entities.active.Structure
import owe.{entities, EffectID, EntityDesirability}

object House extends Structure {

  override def `desirability`: EntityDesirability = (-1, 0, 0, 0, 0, 0)

  override def `subtype`: Structure.Type = Structure.Type.Housing

  override protected def createProperties(): Structure.Properties = Structure.Properties(
    name = "House",
    maxPeople = 5,
    cost = 10
  )

  override protected def createState(): Structure.State = Structure.State(
    risk = Some(Structure.Risk(fire = 0, damage = 0)),
    commodities = Some(Structure.Commodities(available = Map.empty)),
    housing = Some(
      Structure.Housing(
        occupants = 0,
        education = Map.empty,
        entertainment = Map.empty,
        religion = Map.empty,
        healthcare = Map.empty,
        civilService = Map.empty
      )
    ),
    production = None
  )

  override protected def createStateModifiers(): Structure.StateModifiers = Structure.StateModifiers(
    risk = Some(Structure.RiskModifier(fire = 0, damage = 0)),
    commodities = None,
    housing = Some(Structure.HousingModifier()),
    production = None
  )

  override protected def createEffects(): Seq[((Structure.Properties, Structure.State) => Boolean, Effect)] = Seq.empty

  override def `size`: entities.Entity.Size = entities.Entity.Size(height = 1, width = 1)

  override protected def processHousing(
    tickSize: Int,
    state: Option[Structure.Housing],
    modifiers: Option[Structure.HousingModifier]
  ): Option[Structure.Housing] = ??? //TODO

  override protected def processCommodities(
    tickSize: Int,
    state: Option[Structure.Commodities],
    modifiers: Option[Structure.CommoditiesModifier]
  ): Option[Structure.Commodities] = ??? //TODO
}
