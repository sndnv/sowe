package owe.entities.active

import owe.CellDesirability
import owe.entities.ActiveEntity.StructureData
import owe.entities._
import owe.entities.active.behaviour.structure.BaseStructure
import owe.map.grid.Point
import owe.production.{Commodity, CommodityAmount, CommodityAmountModifier}

trait Structure
    extends ActiveEntity[
      Structure.Properties,
      Structure.State,
      Structure.StateModifiers,
      BaseStructure
    ] {
  final override def `type`: Entity.Type = Entity.Type.Structure
}

object Structure {
  trait StructureActorRef extends ActiveEntity.ActiveEntityActorRef

  type Effect = ActiveEntity.Effect[Properties, State, StateModifiers]

  sealed trait PropertiesOnly
  sealed trait StateOnly
  sealed trait StateModifiersOnly

  sealed trait Risk

  case object NoRisk extends Risk with StateOnly with StateModifiersOnly

  case class RiskState(
    fire: RiskAmount,
    damage: RiskAmount
  ) extends Risk
      with StateOnly

  case class RiskModifier(
    fire: RiskAmount, //doc - per tick
    damage: RiskAmount //doc - per tick
  ) extends Risk
      with StateModifiersOnly

  sealed trait Commodities

  case object NoCommodities extends Commodities with StateOnly with StateModifiersOnly

  case class CommoditiesState(
    available: Map[Commodity, CommodityAmount],
    limits: Map[Commodity, CommodityAmount]
  ) extends Commodities
      with StateOnly

  //docs - controls consumption rate for houses && affects production rate
  case class CommoditiesModifier(
    usageRates: Map[Commodity, CommodityAmount] //docs - per occupant (housing) or tick (production)
  ) extends Commodities
      with StateModifiersOnly

  sealed trait Housing

  case object NoHousing extends Housing with StateOnly with StateModifiersOnly

  case class HousingState(
    occupants: Int,
    commodityShortage: Int,
    education: Map[EducationEntry, EducationLevel],
    entertainment: Map[EntertainmentEntry, EntertainmentLevel],
    religion: Map[ReligionEntry, ReligionLevel],
    healthcare: Map[HealthcareEntry, HealthcareLevel],
    civilService: Map[CivilServiceEntry, CivilServiceLevel]
  ) extends Housing
      with StateOnly

  case class HousingModifier(
    education: Map[EducationEntry, EducationLevelModifier],
    entertainment: Map[EntertainmentEntry, EntertainmentLevelModifier],
    religion: Map[ReligionEntry, ReligionLevelModifier],
    healthcare: Map[HealthcareEntry, HealthcareLevelModifier],
    civilService: Map[CivilServiceEntry, CivilServiceLevelModifier]
  ) extends Housing
      with StateModifiersOnly

  sealed trait LabourState
  object LabourState {
    case object None extends LabourState
    case object Looking extends LabourState
    case object Found extends LabourState
  }

  sealed trait WalkerState
  object WalkerState {
    case object NotAvailable extends WalkerState
    case object Available extends WalkerState
  }

  sealed trait Walkers
  case object NoWalkers extends Walkers with PropertiesOnly with StateOnly

  case class WalkersProperties(
    generators: Map[String, StructureData => Option[Walker]]
  ) extends Walkers
      with PropertiesOnly

  case class WalkersState(
    state: Map[String, WalkerState]
  ) extends Walkers
      with StateOnly

  sealed trait Production

  case object NoProduction extends Production with StateOnly with StateModifiersOnly

  case class ProductionState(
    employees: Int,
    labour: LabourState,
    rates: Map[Commodity, CommodityAmount]
  ) extends Production
      with StateOnly

  case class ProductionModifier(
    rates: Map[Commodity, CommodityAmountModifier] //doc - in pct of Production.rate
  ) extends Production
      with StateModifiersOnly

  case class StageProperties(
    maxLife: Life,
    maxPeople: Int,
    minDesirability: CellDesirability,
    commodityShortageLimit: Int
  )

  sealed trait Stages
  case class SingleStage(stage: StageProperties) extends Stages with PropertiesOnly
  case class MultiStage(stages: Seq[StageProperties]) extends Stages with PropertiesOnly

  case object DefaultStage extends Stages with StateOnly
  case class CurrentStage(stage: Int) extends Stages with StateOnly

  case class Properties(
    homePosition: Point,
    name: String,
    walkers: Walkers with PropertiesOnly,
    stages: Stages with PropertiesOnly
  ) extends Entity.Properties

  case class State(
    currentStage: Stages with StateOnly,
    currentLife: Life,
    walkers: Walkers with StateOnly,
    risk: Risk with StateOnly,
    commodities: Commodities with StateOnly,
    housing: Housing with StateOnly,
    production: Production with StateOnly
  ) extends Entity.State

  case class StateModifiers(
    risk: Risk with StateModifiersOnly,
    commodities: Commodities with StateModifiersOnly,
    housing: Housing with StateModifiersOnly,
    production: Production with StateModifiersOnly
  ) extends Entity.StateModifiers
}
