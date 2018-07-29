package owe.entities.active.behaviour.walker

import owe.Tagging._
import owe.entities.ActiveEntity.{ResourceData, StructureData, WalkerData}
import owe.entities.active.{Structure, Walker}
import owe.production.Commodity

object DistributionCalculations {
  def resourceToWalkerTransfer(
    resource: ResourceData,
    walker: WalkerData
  ): Option[DistributionResult] =
    walker.state.commodities match {
      case Walker.CommoditiesState(carrying, limits) =>
        val commodity = resource.properties.commodity
        val resourceAmount = resource.state.currentAmount

        limits.get(commodity).flatMap { limit =>
          val walkerCarrying = carrying.getOrElse(commodity, Commodity.Amount(0))
          val acceptable = (limit - walkerCarrying).max(Commodity.Amount(0))
          val transferable = acceptable.min(resourceAmount)

          if (transferable > Commodity.Amount(0)) {
            Some(
              DistributionResult(
                sourceCommodities = Map(commodity -> transferable * -1),
                targetCommodities = Map(commodity -> transferable)
              )
            )
          } else {
            None
          }
        }

      case _ => None //cannot transfer commodities
    }

  def structureToWalkerTransfer(
    structure: StructureData,
    walker: WalkerData
  ): Option[DistributionResult] =
    (structure.state.commodities, walker.state.commodities) match {
      case (Structure.CommoditiesState(available, _), Walker.CommoditiesState(carrying, limits)) =>
        val result = calculateCommodityUpdates(
          DistributionData(
            available.tag[SourceAvailable],
            carrying.tag[TargetAvailable],
            limits.tag[Limits]
          )
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
            carrying.tag[SourceAvailable],
            available.tag[TargetAvailable],
            limits.tag[Limits]
          )
        )

        if (result.isEmpty) {
          None
        } else {
          Some(result)
        }

      case _ => None //cannot transfer commodities
    }

  private def calculateCommodityUpdates(
    commodities: DistributionData
  ): DistributionResult = {
    val result =
      commodities.sourceAvailable
        .foldLeft(Map.empty[Commodity, Commodity.Amount]) {
          case (distributedCommodities, (commodity, available)) =>
            (commodities.limits.get(commodity), commodities.targetAvailable.get(commodity)) match {
              case (Some(limit), maybeExisting) =>
                val existing = maybeExisting.getOrElse(Commodity.Amount(0))

                val distributionAmount = (limit - existing).min(available)

                distributedCommodities + (commodity -> distributionAmount)

              case _ =>
                distributedCommodities // cannot accept commodity
            }
        }
        .filter(_._2 != Commodity.Amount(0))

    DistributionResult(
      sourceCommodities = result.mapValues(_ * -1),
      targetCommodities = result
    )
  }

  private trait SourceAvailable
  private trait TargetAvailable
  private trait Limits

  private case class DistributionData(
    sourceAvailable: Map[Commodity, Commodity.Amount] @@ SourceAvailable,
    targetAvailable: Map[Commodity, Commodity.Amount] @@ TargetAvailable,
    limits: Map[Commodity, Commodity.Amount] @@ Limits
  )

  case class DistributionResult(
    sourceCommodities: Map[Commodity, Commodity.Amount],
    targetCommodities: Map[Commodity, Commodity.Amount]
  ) {
    def isEmpty: Boolean = targetCommodities.isEmpty || sourceCommodities.isEmpty
  }
}
