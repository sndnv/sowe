package owe.entities.active.behaviour.walker

import owe.Tagging._
import owe.entities.ActiveEntity.{StructureData, WalkerData}
import owe.entities.active.behaviour.walker.BaseDistributor._
import owe.entities.active.{Structure, Walker}
import owe.production.{Commodity, CommodityAmount}

trait BaseDistributor {
  protected def calculateStructureToWalkerTransfer(
    structure: StructureData,
    walker: WalkerData
  ): Option[DistributionResult] =
    (structure.state.commodities, walker.state.commodities) match {
      case (Structure.CommoditiesState(available, limits), Walker.CommoditiesState(carrying, _)) =>
        val result = calculateCommodityUpdates(
          DistributionData(
            carrying.tag[WalkerCarrying],
            available.tag[StructureAvailable],
            limits.tag[Limits]
          ),
          structureToWalkerTransfer
        )

        if (result.isEmpty) {
          None
        } else {
          Some(result)
        }

      case _ => None //cannot transfer commodities
    }

  protected def calculateWalkerToStructureTransfer(
    structure: StructureData,
    walker: WalkerData
  ): Option[DistributionResult] =
    (structure.state.commodities, walker.state.commodities) match {
      case (Structure.CommoditiesState(available, _), Walker.CommoditiesState(carrying, limits)) =>
        val result = calculateCommodityUpdates(
          DistributionData(
            carrying.tag[WalkerCarrying],
            available.tag[StructureAvailable],
            limits.tag[Limits]
          ),
          walkerToStructureTransfer
        )

        if (result.isEmpty) {
          None
        } else {
          Some(result)
        }

      case _ => None //cannot transfer commodities
    }

  private def calculateCommodityUpdates(
    commodities: DistributionData,
    transfer: (
      CommodityAmount @@ StructureAvailable,
      CommodityAmount @@ WalkerCarrying,
      CommodityAmount @@ Limits
    ) => (CommodityAmount, CommodityAmount)
  ): DistributionResult = {
    val result =
      commodities.walkerCarrying.foldLeft(
        (Map.empty[Commodity, CommodityAmount], Map.empty[Commodity, CommodityAmount])) {
        case ((transferableCommodities, remainingCommodities), (commodity, carrying)) =>
          (commodities.limits.get(commodity), commodities.structureAvailable.get(commodity)) match {
            case (Some(limit), maybeExisting) =>
              val existing = maybeExisting.getOrElse(CommodityAmount(0))

              val (transferred, remaining) = transfer(
                existing.tag[StructureAvailable],
                carrying.tag[WalkerCarrying],
                limit.tag[Limits]
              )

              (
                transferableCommodities + (commodity -> transferred),
                remainingCommodities + (commodity -> remaining)
              )

            case _ =>
              (transferableCommodities, remainingCommodities) //cannot accept commodity
          }
      }

    (DistributionResult.apply _).tupled(result)
  }

  private def structureToWalkerTransfer(
    existing: CommodityAmount @@ StructureAvailable,
    carrying: CommodityAmount @@ WalkerCarrying,
    limit: CommodityAmount @@ Limits
  ): (CommodityAmount, CommodityAmount) = {
    val transferred = limit.min(carrying)
    val remaining = carrying + transferred

    (transferred, remaining)
  }

  private def walkerToStructureTransfer(
    existing: CommodityAmount @@ StructureAvailable,
    carrying: CommodityAmount @@ WalkerCarrying,
    limit: CommodityAmount @@ Limits
  ): (CommodityAmount, CommodityAmount) = {
    val transferred = (limit - existing).min(carrying)
    val remaining = carrying - transferred

    (transferred, remaining)
  }
}

object BaseDistributor {
  private trait WalkerCarrying
  private trait StructureAvailable
  private trait Limits

  private case class DistributionData(
    walkerCarrying: Map[Commodity, CommodityAmount] @@ WalkerCarrying,
    structureAvailable: Map[Commodity, CommodityAmount] @@ StructureAvailable,
    limits: Map[Commodity, CommodityAmount] @@ Limits
  )

  case class DistributionResult(
    structureCommodities: Map[Commodity, CommodityAmount],
    walkerCommodities: Map[Commodity, CommodityAmount]
  ) {
    def isEmpty: Boolean = structureCommodities.isEmpty || walkerCommodities.isEmpty
  }
}
