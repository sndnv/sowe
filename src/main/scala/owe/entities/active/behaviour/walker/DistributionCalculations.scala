package owe.entities.active.behaviour.walker

import owe.Tagging._
import owe.entities.ActiveEntity.{StructureData, WalkerData}
import owe.entities.active.{Structure, Walker}
import owe.production.Commodity

object DistributionCalculations {
  def structureToWalkerTransfer(
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
          structureToWalkerTransfer
        )

        if (result.isEmpty) {
          None
        } else {
          Some(result)
        }

      case _ => None //cannot transfer commodities
    }

  def walkerToStructureTransfer(
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
      Commodity.Amount @@ StructureAvailable,
      Commodity.Amount @@ WalkerCarrying,
      Commodity.Amount @@ Limits
    ) => (Commodity.Amount, Commodity.Amount)
  ): DistributionResult = {
    val result =
      commodities.walkerCarrying.foldLeft(
        (Map.empty[Commodity, Commodity.Amount], Map.empty[Commodity, Commodity.Amount])) {
        case ((structureCommodities, walkerCommodities), (commodity, carrying)) =>
          (commodities.limits.get(commodity), commodities.structureAvailable.get(commodity)) match {
            case (Some(limit), maybeExisting) =>
              val existing = maybeExisting.getOrElse(Commodity.Amount(0))

              val (structureAmount, walkerAmount) = transfer(
                existing.tag[StructureAvailable],
                carrying.tag[WalkerCarrying],
                limit.tag[Limits]
              )

              (
                structureCommodities + (commodity -> structureAmount),
                walkerCommodities + (commodity -> walkerAmount)
              )

            case _ =>
              (structureCommodities, walkerCommodities) //cannot accept commodity
          }
      }

    DistributionResult(
      structureCommodities = result._1.filter(_._2 != Commodity.Amount(0)),
      walkerCommodities = result._2.filter(_._2 != Commodity.Amount(0))
    )
  }

  private def structureToWalkerTransfer(
    existing: Commodity.Amount @@ StructureAvailable,
    carrying: Commodity.Amount @@ WalkerCarrying,
    limit: Commodity.Amount @@ Limits
  ): (Commodity.Amount, Commodity.Amount) = {
    val changeAmount = (limit - carrying).min(existing)
    (changeAmount * -1, changeAmount)
  }

  private def walkerToStructureTransfer(
    existing: Commodity.Amount @@ StructureAvailable,
    carrying: Commodity.Amount @@ WalkerCarrying,
    limit: Commodity.Amount @@ Limits
  ): (Commodity.Amount, Commodity.Amount) = {
    val changeAmount = (limit - existing).min(carrying)
    (changeAmount, changeAmount * -1)
  }

  private trait WalkerCarrying
  private trait StructureAvailable
  private trait Limits

  private case class DistributionData(
    walkerCarrying: Map[Commodity, Commodity.Amount] @@ WalkerCarrying,
    structureAvailable: Map[Commodity, Commodity.Amount] @@ StructureAvailable,
    limits: Map[Commodity, Commodity.Amount] @@ Limits
  )

  case class DistributionResult(
    structureCommodities: Map[Commodity, Commodity.Amount],
    walkerCommodities: Map[Commodity, Commodity.Amount]
  ) {
    def isEmpty: Boolean = structureCommodities.isEmpty || walkerCommodities.isEmpty
  }
}
