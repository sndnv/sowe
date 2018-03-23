package owe.entities.active

import owe.entities._
import owe.map.MapCell
import owe.production.{Commodity, CommodityAmount, CommodityUsageRate}

trait Structure
    extends ActiveEntity[
      Structure.Properties,
      Structure.State,
      Structure.StateModifiers,
      Structure.ActorRefTag
    ] {
  final override def `type`: Entity.Type = Entity.Type.Structure

  def `subtype`: Structure.Type

  protected def processRisk(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Option[Structure.Risk],
    modifiers: Option[Structure.RiskModifier]
  ): Option[Structure.Risk] =
    for {
      state <- state
      modifiers <- modifiers
    } yield {
      Structure.Risk(
        fire = state.fire + modifiers.fire * tickSize,
        damage = state.damage + modifiers.damage * tickSize
      )
    }

  protected def processHousing(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Option[Structure.Housing],
    modifiers: Option[Structure.HousingModifier]
  ): Option[Structure.Housing] = None

  protected def processCommodities(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Option[Structure.Commodities],
    modifiers: Option[Structure.CommoditiesModifier]
  ): Option[Structure.Commodities] = None

  protected def processProduction(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    state: Option[Structure.Production],
    modifiers: Option[Structure.ProductionModifier]
  ): Option[Structure.Production] = None

  override protected def tick(
    tickSize: Int,
    cellProperties: MapCell.Properties,
    cellModifiers: MapCell.Modifiers,
    properties: Structure.Properties,
    state: Structure.State,
    modifiers: Structure.StateModifiers
  ): Structure.State =
    //TODO - only do partial updates/copy (lenses?)
    state.copy(
      risk = processRisk(
        tickSize,
        cellProperties,
        cellModifiers,
        state.risk,
        modifiers.risk
      ),
      production = processProduction(
        tickSize,
        cellProperties,
        cellModifiers,
        state.production,
        modifiers.production
      ),
      commodities = processCommodities(
        tickSize,
        cellProperties,
        cellModifiers,
        state.commodities,
        modifiers.commodities
      ),
      housing = processHousing(
        tickSize,
        cellProperties,
        cellModifiers,
        state.housing,
        modifiers.housing
      )
    )
}

object Structure {
  trait ActorRefTag extends ActiveEntity.ActorRefTag

  type Effect = ActiveEntity.Effect[Properties, State, StateModifiers]

  sealed trait Type
  object Type {
    case object Housing extends Type
    case object Entertainment extends Type
    case object Military extends Type
    case object Industry extends Type
    case object Monument extends Type
    case object Religion extends Type
    case object Education extends Type
    case object HealthCare extends Type
    case object CivilService extends Type
  }

  trait HousingStatEntry extends Any
  final case class EducationEntry(name: String) extends AnyVal with HousingStatEntry
  final case class EntertainmentEntry(name: String) extends AnyVal with HousingStatEntry
  final case class ReligionEntry(name: String) extends AnyVal with HousingStatEntry
  final case class HealthcareEntry(name: String) extends AnyVal with HousingStatEntry
  final case class CivilServiceEntry(name: String) extends AnyVal with HousingStatEntry

  trait HousingStatValue extends Any
  final case class EducationLevel(value: Int) extends AnyVal with HousingStatValue
  final case class EntertainmentLevel(value: Int) extends AnyVal with HousingStatValue
  final case class ReligionLevel(value: Int) extends AnyVal with HousingStatValue
  final case class HealthcareLevel(value: Int) extends AnyVal with HousingStatValue
  final case class CivilServiceLevel(value: Int) extends AnyVal with HousingStatValue

  case class Properties(
    name: String,
    maxPeople: Int,
    cost: Int
  ) extends Entity.Properties

  case class Risk(
    fire: Int,
    damage: Int
  )

  case class RiskModifier(
    fire: Int,
    damage: Int
  )

  case class Commodities(
    available: Map[Commodity, CommodityAmount]
  )

  //TODO - controls consumption rate for houses && affects production rate
  case class CommoditiesModifier(
    usageRates: Map[Commodity, CommodityUsageRate]
  )

  case class Housing(
    occupants: Int,
    education: Map[EducationEntry, EducationLevel],
    entertainment: Map[EntertainmentEntry, EntertainmentLevel],
    religion: Map[ReligionEntry, ReligionLevel],
    healthcare: Map[HealthcareEntry, HealthcareLevel],
    civilService: Map[CivilServiceEntry, CivilServiceLevel]
  )

  case class HousingModifier() //TODO

  case class Production(
    employees: Int,
    rate: Int
  )

  case class ProductionModifier(
    rate: Int //doc - in pct of Production.rate
  )

  case class State(
    risk: Option[Risk],
    commodities: Option[Commodities],
    housing: Option[Housing],
    production: Option[Production]
  ) extends Entity.State

  case class StateModifiers(
    risk: Option[RiskModifier],
    commodities: Option[CommoditiesModifier],
    housing: Option[HousingModifier],
    production: Option[ProductionModifier]
  ) extends Entity.StateModifiers
}
