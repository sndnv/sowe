package owe.entities.active.behaviour.structure

import akka.actor.Actor.Receive
import owe._
import owe.entities.ActiveEntity.{ForwardMessage, MapData, ProcessEntityTick, StructureData}
import owe.entities.Entity
import owe.entities.Entity.{State => _, _}
import owe.entities.active.Structure._
import owe.entities.active._
import owe.entities.active.behaviour.BaseBehaviour
import owe.entities.active.behaviour.structure.BaseStructure.{Become, StructureTransition}
import owe.map.GameMap.{CreateEntity, ForwardExchangeMessage}
import owe.production.Exchange.{CommodityAvailable, CommodityRequired, UpdateCommodityState}
import owe.production.{Commodity, CommodityAmount, CommodityAmountModifier, CommodityState}

import scala.concurrent.Future

trait BaseStructure extends BaseBehaviour[Structure.ActorRefTag] {

  import context.dispatcher

  final protected def farming(): Behaviour = {
    case ProcessEntityTick(map, structure: StructureData, messages) =>
      withUpdates(
        structure,
        Seq(
          withProcessedUpdateMessages(_: StructureData, messages),
          withFarming(map, _: StructureData),
          withUpdatedRisk(_: StructureData),
          withGeneratedWalkers(_: StructureData)
        )
      ).foreach { updatedData =>
        calculateProduction(structure)
          .foreach(updateExchangeState(_, CommodityState.Produced))

        (structure.state.commodities, updatedData.state.commodities) match {
          case (CommoditiesState(current, _), CommoditiesState(updated, _)) =>
            updateExchangeStatsWithAvailableCommodities(structure.properties.id, current, updated)

          case _ => //do nothing
        }

        self ! Become(farming, updatedData)
      }
  }

  final protected def housing(): Behaviour = {
    case ProcessEntityTick(map, structure: StructureData, messages) =>
      withUpdates(
        structure,
        Seq(
          withProcessedUpdateMessages(_: StructureData, messages),
          withProcessedHousing(_: StructureData),
          withProducedResources(_: StructureData),
          withConsumedResources(_: StructureData),
          withUpdatedRisk(_: StructureData),
          withProcessedTransition(map, _: StructureData),
          withGeneratedWalkers(_: StructureData)
        )
      ).foreach { updatedData =>
        calculateConsumption(structure)
          .foreach(updateExchangeState(_, CommodityState.Used))

        calculateRequiredCommodities(structure)
          .foreach(updateExchangeStatsWithRequiredCommodities(structure.properties.id, _))

        self ! Become(housing, updatedData)
      }
  }

  final protected def producing(): Behaviour = {
    case ProcessEntityTick(map, structure: StructureData, messages) =>
      withUpdates(
        structure,
        Seq(
          withProcessedUpdateMessages(_: StructureData, messages),
          withProducedResources(_: StructureData),
          withConsumedResources(_: StructureData),
          withUpdatedRisk(_: StructureData),
          withProcessedTransition(map, _: StructureData),
          withGeneratedWalkers(_: StructureData)
        )
      ).foreach { updatedData =>
        calculateProduction(structure)
          .foreach(updateExchangeState(_, CommodityState.Produced))

        calculateConsumption(structure)
          .foreach(updateExchangeState(_, CommodityState.Used))

        calculateRequiredCommodities(structure)
          .foreach(updateExchangeStatsWithRequiredCommodities(structure.properties.id, _))

        (structure.state.commodities, updatedData.state.commodities) match {
          case (CommoditiesState(current, _), CommoditiesState(updated, _)) =>
            updateExchangeStatsWithAvailableCommodities(structure.properties.id, current, updated)

          case _ => //do nothing
        }

        self ! Become(producing, updatedData)
      }
  }

  private def base(): Behaviour = {
    case Become(behaviour, structure) =>
      parent ! structure.state
      become(behaviour, structure)
  }

  private def become(behaviour: () => Behaviour, structure: StructureData): Unit =
    if (structure.state.currentLife.isSufficient) {
      context.become(base().orElse(behaviour()))
    } else {
      structure.state.commodities match {
        case CommoditiesState(available, _) =>
          updateExchangeState(available.filter(_._2 > CommodityAmount(0)), CommodityState.Lost)

        case _ => //do nothing
      }
    }

  private def updateExchangeState(commodities: Map[Commodity, CommodityAmount], state: CommodityState): Unit =
    commodities.foreach {
      case (commodity, amount) =>
        parent ! ForwardMessage(
          ForwardExchangeMessage(
            UpdateCommodityState(commodity, amount, state)
          )
        )
    }

  private def updateExchangeStatsWithAvailableCommodities(
    entityID: EntityID,
    initialCommodities: Map[Commodity, CommodityAmount],
    updatedCommodities: Map[Commodity, CommodityAmount]
  ): Unit =
    updatedCommodities.foreach {
      case (commodity, updatedAmount) =>
        val currentAmount = initialCommodities.getOrElse(commodity, CommodityAmount(0))

        if (currentAmount != updatedAmount) {
          parent ! ForwardMessage(
            ForwardExchangeMessage(
              CommodityAvailable(commodity, updatedAmount, entityID)
            )
          )
        }
    }

  private def updateExchangeStatsWithRequiredCommodities(
    entityID: EntityID,
    consumedCommodities: Map[Commodity, CommodityAmount]
  ): Unit =
    consumedCommodities.foreach {
      case (commodity, amount) =>
        parent ! ForwardMessage(
          ForwardExchangeMessage(
            CommodityRequired(commodity, amount, entityID)
          )
        )
    }

  private def withGeneratedWalkers(structure: StructureData): Future[State] = {
    val updatedState: State = (structure.properties.walkers, structure.state.walkers) match {
      case (WalkersProperties(generators), WalkersState(state)) =>
        generators.foldLeft(structure.state) {
          case (currentState, (walkerName, walkerGenerator)) =>
            walkerGenerator(structure) match {
              case Some(walker) =>
                parent ! ForwardMessage(CreateEntity(walker, structure.properties.homePosition))
                currentState.copy(walkers = WalkersState(state + (walkerName -> WalkerState.Available)))

              case None => currentState //do nothing
            }
        }

      case _ => structure.state //can't generate walkers; data missing
    }

    Future.successful(updatedState)
  }

  private def withProcessedUpdateMessages(
    structure: StructureData,
    pendingMessages: Seq[Entity.Message]
  ): Future[State] =
    Future.successful(
      pendingMessages.foldLeft(structure.state) {
        case (currentState, message) =>
          message match {
            case ProcessCommodities(commodities) =>
              currentState.commodities match {
                case CommoditiesState(available, limits) =>
                  val occupants = currentState.housing match {
                    case housing: HousingState => housing.occupants
                    case NoHousing             => 0
                  }

                  val updatedCommodities = available ++ commodities.map {
                    case (commodity, amount) =>
                      val limitAmount = limits.getOrElse(commodity, CommodityAmount(0))
                      val updatedCommodityAmount =
                        (available.getOrElse(commodity, CommodityAmount(0)) + amount)
                          .min(if (occupants > 0) limitAmount * occupants else limitAmount)
                          .max(CommodityAmount(0))

                      (commodity, updatedCommodityAmount)
                  }
                  currentState.copy(commodities = CommoditiesState(updatedCommodities, limits))

                case NoCommodities => currentState //can't update commodities; data missing
              }

            case ProcessOccupantsUpdate(occupants) =>
              currentState.housing match {
                case housing: HousingState =>
                  val maxPeople = (currentState.currentStage, structure.properties.stages) match {
                    case (DefaultStage, SingleStage(stage))                               => stage.maxPeople
                    case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage => stages(stage).maxPeople
                    case _                                                                => 0 //stage data missing
                  }

                  currentState.copy(
                    housing = housing.copy(
                      occupants = (housing.occupants + occupants).min(maxPeople).max(0)
                    )
                  )

                case NoHousing => currentState //can't update housing; data missing
              }

            case ProcessLabourUpdate(employees) =>
              currentState.production match {
                case production: ProductionState =>
                  val maxPeople = (currentState.currentStage, structure.properties.stages) match {
                    case (DefaultStage, SingleStage(stage))                               => stage.maxPeople
                    case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage => stages(stage).maxPeople
                    case _                                                                => 0 //stage data missing
                  }

                  currentState.copy(
                    production = production.copy(
                      employees = (production.employees + employees).min(maxPeople).max(0)
                    )
                  )

                case NoProduction => currentState //can't update labour; data missing
              }

            case ProcessAttack(damage) =>
              currentState.copy(currentLife = damage(currentState.currentLife))

            case ProcessLabourFound() =>
              currentState.production match {
                case production: ProductionState =>
                  production.labour match {
                    case LabourState.Looking | LabourState.None =>
                      currentState.copy(production = production.copy(labour = LabourState.Found))

                    case LabourState.Found => currentState //do nothing; labour already found
                  }

                case _ => currentState //can't update labour; not necessary or data missing
              }
          }
      }
    )

  private def withUpdatedRisk(structure: StructureData): Future[State] = {
    val state: State = (structure.state.risk, structure.modifiers.risk) match {
      case (state: RiskState, modifiers: RiskModifier) =>
        structure.state.copy(
          risk = RiskState(
            fire = state.fire + modifiers.fire,
            damage = state.damage + modifiers.damage
          )
        )

      case _ => structure.state //can't calculate risk; risk data missing
    }

    Future.successful(state)
  }

  private def withFarming(map: MapData, structure: StructureData): Future[State] = {
    val updatedState: State =
      structure.state.commodities match {
        case CommoditiesState(available, limits) =>
          calculateProduction(structure) match {
            case Some(actualProductionRates) =>
              val updatedResources = available ++ actualProductionRates.map {
                case (commodity, amount) =>
                  val water = map.cellModifiers.water(map.cellProperties.water).min(Water.Max)
                  val fertility = map.cellModifiers.fertility(map.cellProperties.fertility).min(Fertility.Max)

                  val actualAmount = amount.basedOn(fertility.basedOn(water))

                  (commodity, available.getOrElse(commodity, CommodityAmount(0)) + actualAmount)
              }

              structure.state.copy(
                commodities = CommoditiesState(updatedResources, limits)
              )

            case None => structure.state
          }

        case _ => structure.state
      }

    Future.successful(updatedState)
  }

  private def withProducedResources(structure: StructureData): Future[State] = {
    val updatedState: State =
      structure.state.commodities match {
        case CommoditiesState(available, limits) =>
          calculateProduction(structure) match {
            case Some(actualProductionRates) =>
              val updatedResources = available ++ actualProductionRates.map {
                case (commodity, amount) =>
                  (commodity, available.getOrElse(commodity, CommodityAmount(0)) + amount)
              }

              structure.state.copy(
                commodities = CommoditiesState(updatedResources, limits)
              )

            case None => structure.state
          }

        case _ => structure.state
      }

    Future.successful(updatedState)
  }

  private def withConsumedResources(structure: StructureData): Future[State] = {
    val updatedState: State = (structure.state.commodities, structure.modifiers.commodities) match {
      case (CommoditiesState(available, limits), _: CommoditiesModifier) =>
        calculateConsumption(structure) match {
          case Some(consumedCommodities) =>
            val updatedResources = available ++ consumedCommodities.map {
              case (commodity, amount) =>
                (commodity, available.getOrElse(commodity, CommodityAmount(0)) - amount)
            }

            structure.state.copy(
              commodities = CommoditiesState(updatedResources, limits)
            )

          case None => structure.state //can't update commodities; data missing
        }

      case _ => structure.state //can't update commodities; data missing
    }

    Future.successful(updatedState)
  }

  private def withProcessedHousing(structure: StructureData): Future[State] = {
    val updatedState: State = (structure.state.housing, structure.modifiers.housing) match {
      case (housing @ HousingState(_, commodityShortage, education, entertainment, religion, healthcare, civilService),
            HousingModifier(educationModifier,
                            entertainmentModifier,
                            religionModifier,
                            healthcareModifier,
                            civilServiceModifier)) =>
        val updatedEducation = education.map {
          case (entry, level) =>
            val actualModifier = educationModifier.getOrElse(entry, EducationLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }

        val updatedEntertainment = entertainment.map {
          case (entry, level) =>
            val actualModifier = entertainmentModifier.getOrElse(entry, EntertainmentLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }
        val updatedReligion = religion.map {
          case (entry, level) =>
            val actualModifier = religionModifier.getOrElse(entry, ReligionLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }
        val updatedHealthcare = healthcare.map {
          case (entry, level) =>
            val actualModifier = healthcareModifier.getOrElse(entry, HealthcareLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }
        val updatedCivilService = civilService.map {
          case (entry, level) =>
            val actualModifier = civilServiceModifier.getOrElse(entry, CivilServiceLevelModifier(0)).value - 1
            val updatedLevel = level.copy(current = level.current + actualModifier)
            (entry, updatedLevel)
        }

        structure.state.copy(
          housing = housing.copy(
            commodityShortage = if (areHousingCommoditiesMissing(structure)) commodityShortage + 1 else 0,
            education = updatedEducation,
            entertainment = updatedEntertainment,
            religion = updatedReligion,
            healthcare = updatedHealthcare,
            civilService = updatedCivilService
          )
        )

      case _ => structure.state //can't update housing; data missing
    }

    Future.successful(updatedState)
  }

  private def withProcessedTransition(map: MapData, structure: StructureData): Future[State] = {
    val updatedState: State = (structure.state.currentStage, structure.properties.stages) match {
      case (CurrentStage(stage), MultiStage(stages)) =>
        calculateStructureTransition(map, structure) match {
          case StructureTransition.Upgrade =>
            val nextStage = stage + 1
            if (nextStage >= stages.size) {
              structure.state
            } else {
              structure.state.copy(currentStage = CurrentStage(nextStage))
            }

          case StructureTransition.Downgrade =>
            val nextStage = stage - 1
            if (nextStage >= stages.size || nextStage < 0) {
              structure.state
            } else {
              structure.state.copy(currentStage = CurrentStage(nextStage))
            }

          case StructureTransition.None =>
            structure.state //no transition needed
        }

      case _ =>
        structure.state //no transition needed
    }

    Future.successful(updatedState)
  }

  private def calculateStructureTransition(map: MapData, structure: StructureData): StructureTransition =
    structure.state.housing match {
      case (HousingState(occupants, commodityShortage, education, entertainment, religion, healthcare, civilService)) =>
        if (occupants > 0) {
          val commoditiesMissing = areHousingCommoditiesMissing(structure)
          val cellDesirability =
            map.cellModifiers.desirability(map.cellProperties.desirability).min(CellDesirability.Max)

          val (minDesirability, commodityShortageLimit) =
            (structure.state.currentStage, structure.properties.stages) match {
              case (DefaultStage, SingleStage(stage)) =>
                (stage.minDesirability, stage.commodityShortageLimit)

              case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage =>
                (stages(stage).minDesirability, stages(stage).commodityShortageLimit)

              case _ =>
                (CellDesirability.Neutral, 0) //stage data missing
            }

          val enoughDesirability = cellDesirability >= minDesirability

          val shouldDowngrade =
            (commoditiesMissing && commodityShortage >= commodityShortageLimit) ||
              !enoughDesirability ||
              education.forall { case (_, level)     => level.current < level.minimal } ||
              entertainment.forall { case (_, level) => level.current < level.minimal } ||
              religion.forall { case (_, level)      => level.current < level.minimal } ||
              healthcare.forall { case (_, level)    => level.current < level.minimal } ||
              civilService.forall { case (_, level)  => level.current < level.minimal }

          if (shouldDowngrade) {
            StructureTransition.Downgrade
          } else {
            val shouldUpgrade =
              !commoditiesMissing &&
                enoughDesirability &&
                education.forall { case (_, level)     => level.current >= level.required } &&
                entertainment.forall { case (_, level) => level.current >= level.required } &&
                religion.forall { case (_, level)      => level.current >= level.required } &&
                healthcare.forall { case (_, level)    => level.current >= level.required } &&
                civilService.forall { case (_, level)  => level.current >= level.required }

            if (shouldUpgrade) {
              StructureTransition.Upgrade
            } else {
              StructureTransition.None //no transition; no need to upgrade or downgrade
            }
          }

        } else {
          StructureTransition.None //no transition; no occupants
        }

      case _ =>
        val cellDesirability =
          map.cellModifiers.desirability(map.cellProperties.desirability).min(CellDesirability.Max)

        val minDesirability =
          (structure.state.currentStage, structure.properties.stages) match {
            case (DefaultStage, SingleStage(stage)) =>
              stage.minDesirability

            case (CurrentStage(stage), MultiStage(stages)) if stages.size > stage =>
              stages(stage).minDesirability

            case _ =>
              CellDesirability.Neutral //stage data missing
          }

        if (cellDesirability > minDesirability) {
          StructureTransition.Upgrade
        } else if (cellDesirability < minDesirability) {
          StructureTransition.Downgrade
        } else {
          StructureTransition.None
        }
    }

  private def calculateProduction(
    structure: StructureData
  ): Option[Map[production.Commodity, CommodityAmount]] =
    (structure.state.commodities,
     structure.modifiers.commodities,
     structure.state.production,
     structure.modifiers.production) match {
      case (CommoditiesState(available, limits),
            CommoditiesModifier(usageRates),
            ProductionState(employees, _, productionRates),
            ProductionModifier(productionModifierRates)) =>
        val hasEnoughCommodities = usageRates.forall {
          case (commodity, amount) =>
            available.get(commodity).exists(_ >= (amount * employees))
        }

        if (hasEnoughCommodities) {
          val actualProductionRates = productionRates.map {
            case (commodity, amount) =>
              val productionModifier = productionModifierRates.getOrElse(commodity, CommodityAmountModifier(100))
              val amountModifier = productionModifier * employees

              (commodity, amountModifier(amount))
          }

          val isWithinLimits = actualProductionRates.forall {
            case (commodity, amount) =>
              limits.get(commodity).exists(_ >= amount)
          }

          if (isWithinLimits) {
            Some(actualProductionRates)
          } else {
            None //limits reached
          }
        } else {
          None //not enough commodities
        }

      case _ => None //data missing
    }

  private def calculateConsumption(
    structure: StructureData
  ): Option[Map[production.Commodity, CommodityAmount]] =
    (structure.state.commodities, structure.modifiers.commodities) match {
      case (CommoditiesState(available, _), CommoditiesModifier(usageRates)) =>
        val people = (structure.state.housing, structure.state.production) match {
          case (housing: HousingState, NoProduction)    => housing.occupants
          case (NoHousing, production: ProductionState) => production.employees
          case _                                        => 0
        }

        if (people > 0) {
          val consumedCommodities = usageRates.collect {
            case (commodity, amount) if available.get(commodity).exists(_ >= (amount * people)) =>
              (commodity, amount)
          }

          if (consumedCommodities.nonEmpty && consumedCommodities.size == usageRates.size) {
            Some(consumedCommodities)
          } else {
            None //not enough commodities
          }
        } else {
          None //no employees or occupants or consumption data is missing
        }

      case _ => None //data missing
    }

  private def calculateRequiredCommodities(
    structure: StructureData
  ): Option[Map[production.Commodity, CommodityAmount]] =
    (structure.state.commodities, structure.modifiers.commodities) match {
      case (CommoditiesState(available, limits), CommoditiesModifier(usageRates)) =>
        val requiredCommodities = usageRates
          .map {
            case (commodity, _) =>
              val commodityAvailable = available.getOrElse(commodity, CommodityAmount(0))
              val commodityLimit = limits.getOrElse(commodity, CommodityAmount(0))

              (commodity, commodityLimit - commodityAvailable)
          }
          .filter(_._2 > CommodityAmount(0))

        Some(requiredCommodities)

      case _ => None //data missing
    }

  private def areHousingCommoditiesMissing(structure: StructureData): Boolean =
    structure.state.commodities match {
      case CommoditiesState(available, _) =>
        calculateRequiredCommodities(structure) match {
          case Some(requiredCommodities) =>
            requiredCommodities.exists {
              case (commodity, required) =>
                !available.get(commodity).exists(_ >= required)
            }

          case None => false //data missing
        }

      case _ => false //data missing
    }
}

object BaseStructure {
  private sealed trait StructureTransition
  private case class Become(behaviour: () => Receive, structure: StructureData)
  private object StructureTransition {
    case object Upgrade extends StructureTransition
    case object Downgrade extends StructureTransition
    case object None extends StructureTransition
  }
}
