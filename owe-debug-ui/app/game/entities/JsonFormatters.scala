package game.entities

import owe.entities.ActiveEntity.{Data, StructureData}
import owe.entities.active.Structure._
import owe.entities.active.Walker._
import owe.entities.active._
import owe.entities.active.attributes._
import owe.entities.{ActiveEntity, Entity}
import owe.events.Event.{CellEvent, EntityEvent, Identifier, SystemEvent}
import owe.map.grid.Point
import owe.map.{Cell, MapEntity}
import owe.production.Commodity
import owe.production.Exchange.{ExchangeCommodities, ExchangeEntities, ExchangeStats}
import play.api.libs.json._

object JsonFormatters {
  def writesString[T]: Format[T] = Format(
    Reads[T](_ => ???),
    Writes[T](obj => JsString(obj.toString))
  )

  def asMap[A, B, T <: Map[A, B]](implicit a: Format[A], b: Format[B]): Format[T] = Format(
    Reads[T](_ => ???),
    Writes[T](obj =>
      Json.toJson(obj.map {
        case (key, value) =>
          (Json.toJson(key).toString, Json.toJson(value))
      }))
  )

  implicit def asOpt[T](implicit ft: Format[T]): Format[Option[T]] = Format(
    Reads[Option[T]](_.validateOpt[T]),
    Writes[Option[T]] {
      case Some(t) => Json.toJson(t)
      case None    => JsNull
    }
  )

  implicit val sizeFormat: Format[Entity.Size] = Json.format[Entity.Size]
  implicit val cellDesirabilityFormat: Format[Cell.Desirability] = Json.format[Cell.Desirability]
  implicit val entityDesirabilityFormat: Format[Entity.Desirability] = Json.format[Entity.Desirability]

  implicit val attackRate: Format[AttackRate] = Json.format[AttackRate]
  implicit val attackRateModifier: Format[AttackRate.Modifier] = Json.format[AttackRate.Modifier]
  implicit val attackDamage: Format[AttackDamage] = Json.format[AttackDamage]
  implicit val attackDamageModifier: Format[AttackDamage.Modifier] = Json.format[AttackDamage.Modifier]
  implicit val distance: Format[Distance] = Json.format[Distance]
  implicit val distanceModifier: Format[Distance.Modifier] = Json.format[Distance.Modifier]

  implicit val speed: Format[Speed] = Json.format[Speed]
  implicit val speedModifier: Format[Speed.Modifier] = Json.format[Speed.Modifier]

  implicit val point: Format[Point] = Json.format[Point]
  implicit val commodity: Format[Commodity] = Json.format[Commodity]
  implicit val commodityAmount: Format[Commodity.Amount] = Json.format[Commodity.Amount]
  implicit val commodityAmountModifier: Format[Commodity.AmountModifier] = Json.format[Commodity.AmountModifier]

  implicit def entityRef[T <: Entity.EntityRef]: Format[T] = writesString

  implicit val commodityMap: Format[Map[Commodity, Commodity.Amount]] =
    asMap[Commodity, Commodity.Amount, Map[Commodity, Commodity.Amount]]

  implicit val commodityModifiersMap: Format[Map[Commodity, Commodity.AmountModifier]] =
    asMap[Commodity, Commodity.AmountModifier, Map[Commodity, Commodity.AmountModifier]]

  implicit val educationEntry: Format[EducationEntry] = Json.format[EducationEntry]
  implicit val educationLevel: Format[EducationLevel] = Json.format[EducationLevel]
  implicit val educationLevelModifier: Format[EducationLevelModifier] = Json.format[EducationLevelModifier]

  implicit val entertainmentEntry: Format[EntertainmentEntry] = Json.format[EntertainmentEntry]
  implicit val entertainmentLevel: Format[EntertainmentLevel] = Json.format[EntertainmentLevel]
  implicit val entertainmentLevelModifier: Format[EntertainmentLevelModifier] = Json.format[EntertainmentLevelModifier]

  implicit val religionEntry: Format[ReligionEntry] = Json.format[ReligionEntry]
  implicit val religionLevel: Format[ReligionLevel] = Json.format[ReligionLevel]
  implicit val religionLevelModifier: Format[ReligionLevelModifier] = Json.format[ReligionLevelModifier]

  implicit val healthcareEntry: Format[HealthcareEntry] = Json.format[HealthcareEntry]
  implicit val healthcareLevel: Format[HealthcareLevel] = Json.format[HealthcareLevel]
  implicit val healthcareLevelModifier: Format[HealthcareLevelModifier] = Json.format[HealthcareLevelModifier]

  implicit val civilServiceEntry: Format[CivilServiceEntry] = Json.format[CivilServiceEntry]
  implicit val civilServiceLevel: Format[CivilServiceLevel] = Json.format[CivilServiceLevel]
  implicit val civilServiceLevelModifier: Format[CivilServiceLevelModifier] = Json.format[CivilServiceLevelModifier]

  implicit val educationMap: Format[Map[EducationEntry, EducationLevel]] =
    asMap[EducationEntry, EducationLevel, Map[EducationEntry, EducationLevel]]

  implicit val educationModifiersMap: Format[Map[EducationEntry, EducationLevelModifier]] =
    asMap[EducationEntry, EducationLevelModifier, Map[EducationEntry, EducationLevelModifier]]

  implicit val entertainmentMap: Format[Map[EntertainmentEntry, EntertainmentLevel]] =
    asMap[EntertainmentEntry, EntertainmentLevel, Map[EntertainmentEntry, EntertainmentLevel]]

  implicit val entertainmentModifiersMap: Format[Map[EntertainmentEntry, EntertainmentLevelModifier]] =
    asMap[EntertainmentEntry, EntertainmentLevelModifier, Map[EntertainmentEntry, EntertainmentLevelModifier]]

  implicit val religionMap: Format[Map[ReligionEntry, ReligionLevel]] =
    asMap[ReligionEntry, ReligionLevel, Map[ReligionEntry, ReligionLevel]]

  implicit val religionModifiersMap: Format[Map[ReligionEntry, ReligionLevelModifier]] =
    asMap[ReligionEntry, ReligionLevelModifier, Map[ReligionEntry, ReligionLevelModifier]]

  implicit val healthcareMap: Format[Map[HealthcareEntry, HealthcareLevel]] =
    asMap[HealthcareEntry, HealthcareLevel, Map[HealthcareEntry, HealthcareLevel]]

  implicit val healthcareModifiersMap: Format[Map[HealthcareEntry, HealthcareLevelModifier]] =
    asMap[HealthcareEntry, HealthcareLevelModifier, Map[HealthcareEntry, HealthcareLevelModifier]]

  implicit val civilServiceMap: Format[Map[CivilServiceEntry, CivilServiceLevel]] =
    asMap[CivilServiceEntry, CivilServiceLevel, Map[CivilServiceEntry, CivilServiceLevel]]

  implicit val civilServiceModifiersMap: Format[Map[CivilServiceEntry, CivilServiceLevelModifier]] =
    asMap[CivilServiceEntry, CivilServiceLevelModifier, Map[CivilServiceEntry, CivilServiceLevelModifier]]

  implicit val structureWalkerState: Format[WalkerState] = writesString

  implicit val walkerStateMap: Format[Map[String, WalkerState]] =
    asMap[String, WalkerState, Map[String, WalkerState]]

  implicit val walkerGenerator: Format[StructureData => Option[Walker]] = writesString

  implicit val walkersGenerators: Format[Map[String, StructureData => Option[Walker]]] =
    asMap[String, StructureData => Option[Walker], Map[String, StructureData => Option[Walker]]]

  implicit val attackTarget: Format[Data => Boolean] = writesString

  implicit val life: Format[Life] = Json.format[Life]
  implicit val riskAmount: Format[RiskAmount] = Json.format[RiskAmount]
  implicit val riskState: Format[RiskState] = Json.format[RiskState]
  implicit val riskModifier: Format[RiskModifier] = Json.format[RiskModifier]
  implicit val noRisk: Format[Structure.NoRisk.type] = writesString
  implicit val risk: Format[Risk] = Json.format[Risk]
  implicit val structureNoCommodities: Format[Structure.NoCommodities.type] = writesString
  implicit val structureCommoditiesState: Format[Structure.CommoditiesState] = Json.format[Structure.CommoditiesState]
  implicit val structureCommoditiesModifier: Format[CommoditiesModifier] = Json.format[Structure.CommoditiesModifier]
  implicit val structureCommodities: Format[Structure.Commodities] = Json.format[Structure.Commodities]
  implicit val noHousing: Format[Structure.NoHousing.type] = writesString
  implicit val housingState: Format[HousingState] = Json.format[HousingState]
  implicit val housingModifier: Format[HousingModifier] = Json.format[HousingModifier]
  implicit val housing: Format[Housing] = Json.format[Housing]
  implicit val labourState: Format[LabourState] = writesString
  implicit val noWalkers: Format[Structure.NoWalkers.type] = writesString
  implicit val walkersProperties: Format[WalkersProperties] = Json.format[WalkersProperties]
  implicit val walkersState: Format[WalkersState] = Json.format[WalkersState]
  implicit val walkers: Format[Walkers] = Json.format[Walkers]
  implicit val noProduction: Format[Structure.NoProduction.type] = writesString
  implicit val productionState: Format[ProductionState] = Json.format[ProductionState]
  implicit val productionModifier: Format[ProductionModifier] = Json.format[ProductionModifier]
  implicit val production: Format[Production] = Json.format[Production]
  implicit val stageProperties: Format[StageProperties] = Json.format[StageProperties]
  implicit val singleStage: Format[SingleStage] = Json.format[SingleStage]
  implicit val multiStage: Format[MultiStage] = Json.format[MultiStage]
  implicit val defaultStage: Format[Structure.DefaultStage.type] = writesString
  implicit val currentStage: Format[CurrentStage] = Json.format[CurrentStage]
  implicit val stages: Format[Stages] = Json.format[Stages]

  implicit val walkerNoCommodities: Format[Walker.NoCommodities.type] = writesString
  implicit val walkerCommoditiesState: Format[Walker.CommoditiesState] = Json.format[Walker.CommoditiesState]
  implicit val walkerCommodities: Format[Walker.Commodities] = Json.format[Walker.Commodities]

  implicit val noAttack: Format[Walker.NoAttack.type] = writesString
  implicit val attackProperties: Format[AttackProperties] = Json.format[AttackProperties]
  implicit val attackModifiers: Format[AttackModifiers] = Json.format[AttackModifiers]
  implicit val attack: Format[Attack] = Json.format[Attack]

  implicit val movementMode: Format[MovementMode] = writesString
  implicit val traversalMode: Format[TraversalMode] = writesString
  implicit val spawnLocation: Format[SpawnLocation] = writesString

  implicit val resourceProperties: Format[Resource.Properties] = Json.format[Resource.Properties]
  implicit val resourceState: Format[Resource.State] = Json.format[Resource.State]
  implicit val resourceStateModifiers: Format[Resource.StateModifiers] = Json.format[Resource.StateModifiers]

  implicit val structureProperties: Format[Structure.Properties] = Format(
    Reads[Structure.Properties](_ => ???),
    Writes[Structure.Properties](
      obj =>
        Json.obj(
          "homePosition" -> Json.toJson(obj.homePosition),
          "name" -> Json.toJson(obj.name),
          "walkers" -> Json.toJson(obj.walkers),
          "stages" -> Json.toJson(obj.stages)
      )
    )
  )

  implicit val structureState: Format[Structure.State] = Format(
    Reads[Structure.State](_ => ???),
    Writes[Structure.State](
      obj =>
        Json.obj(
          "currentStage" -> Json.toJson(obj.currentStage),
          "currentLife" -> Json.toJson(obj.currentLife),
          "walkers" -> Json.toJson(obj.walkers),
          "risk" -> Json.toJson(obj.risk),
          "commodities" -> Json.toJson(obj.commodities),
          "housing" -> Json.toJson(obj.housing),
          "production" -> Json.toJson(obj.production)
      ))
  )

  implicit val structureStateModifiers: Format[Structure.StateModifiers] = Format(
    Reads[Structure.StateModifiers](_ => ???),
    Writes[Structure.StateModifiers](
      obj =>
        Json.obj(
          "risk" -> Json.toJson(obj.risk),
          "commodities" -> Json.toJson(obj.commodities),
          "housing" -> Json.toJson(obj.housing),
          "production" -> Json.toJson(obj.production)
      ))
  )

  implicit val walkerProperties: Format[Walker.Properties] = Format(
    Reads[Walker.Properties](_ => ???),
    Writes[Walker.Properties](
      obj =>
        Json.obj(
          "parent" -> Json.toJson(obj.parent),
          "homePosition" -> Json.toJson(obj.homePosition),
          "name" -> Json.toJson(obj.name),
          "maxLife" -> Json.toJson(obj.maxLife),
          "movementSpeed" -> Json.toJson(obj.movementSpeed),
          "maxRoamingDistance" -> Json.toJson(obj.maxRoamingDistance),
          "attack" -> Json.toJson(obj.attack),
          "traversalMode" -> Json.toJson(obj.traversalMode),
      )
    )
  )

  implicit val walkerState: Format[Walker.State] = Format(
    Reads[Walker.State](_ => ???),
    Writes[Walker.State](
      obj =>
        Json.obj(
          "currentPosition" -> Json.toJson(obj.currentPosition),
          "currentLife" -> Json.toJson(obj.currentLife),
          "distanceCovered" -> Json.toJson(obj.distanceCovered),
          "commodities" -> Json.toJson(obj.commodities),
          "path" -> Json.toJson(obj.path),
          "mode" -> Json.toJson(obj.mode),
      )
    )
  )

  implicit val walkerStateModifiers: Format[Walker.StateModifiers] = Format(
    Reads[Walker.StateModifiers](_ => ???),
    Writes[Walker.StateModifiers](
      obj =>
        Json.obj(
          "movementSpeed" -> Json.toJson(obj.movementSpeed),
          "maxRoamingDistance" -> Json.toJson(obj.maxRoamingDistance),
          "attack" -> Json.toJson(obj.attack)
      )
    )
  )

  implicit val resourceData: Format[ActiveEntity.ResourceData] = Json.format[ActiveEntity.ResourceData]
  implicit val structureData: Format[ActiveEntity.StructureData] = Json.format[ActiveEntity.StructureData]
  implicit val walkerData: Format[ActiveEntity.WalkerData] = Json.format[ActiveEntity.WalkerData]
  implicit val activeEntityData: Format[ActiveEntity.Data] = Json.format[ActiveEntity.Data]

  implicit val mapEntityFormat: Format[MapEntity] = Json.format[MapEntity]

  implicit val commoditiesFormat: Format[ExchangeCommodities] = Format(
    Reads[ExchangeCommodities](_ => ???),
    Writes[ExchangeCommodities] { commodities =>
      Json.obj(
        "required" -> Json.toJson(
          commodities.required.map(e => (s"(${e._1._1.name},${e._1._2.toString})", e._2.value))
        ),
        "available" -> Json.toJson(
          commodities.available.map(e => (s"(${e._1._1.name},${e._1._2.toString})", e._2.value))
        ),
        "inTransit" -> Json.toJson(
          commodities.inTransit.map(e =>
            (s"(${e._1._1.name},${e._1._2.toString})", s"(${e._2._1.value},${e._2._2.toString})"))
        )
      )
    }
  )

  implicit val statsFormat: Format[ExchangeStats] = Format(
    Reads[ExchangeStats](_ => ???),
    Writes[ExchangeStats] { stats =>
      Json.obj(
        "produced" -> Json.toJson(stats.produced.map(e => (e._1.name, e._2.value))),
        "used" -> Json.toJson(stats.used.map(e => (e._1.name, e._2.value))),
        "lost" -> Json.toJson(stats.lost.map(e => (e._1.name, e._2.value)))
      )
    }
  )

  implicit val entitiesFormat: Format[ExchangeEntities] = Format(
    Reads[ExchangeEntities](_ => ???),
    Writes[ExchangeEntities] { entities =>
      Json.obj(
        "producers" -> Json.toJson(entities.producers.map(e => (e._1.name, e._2))),
        "consumers" -> Json.toJson(entities.consumers.map(e => (e._1.name, e._2)))
      )
    }
  )

  implicit val eventIdentifier: Format[Identifier] = writesString

  implicit val systemEvent: Format[SystemEvent] = Json.format[SystemEvent]
  implicit val cellEvent: Format[CellEvent] = Json.format[CellEvent]
  implicit val entityEvent: Format[EntityEvent] = Json.format[EntityEvent]
}
