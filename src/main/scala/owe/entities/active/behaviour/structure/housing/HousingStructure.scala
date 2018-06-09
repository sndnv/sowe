package owe.entities.active.behaviour.structure.housing

import owe.entities.ActiveEntity.{ProcessEntityTick, StructureData}
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.structure.BaseStructure.Become
import owe.entities.active.behaviour.structure.transformations._
import owe.entities.active.behaviour.structure.{BaseStructure, CommodityCalculations}
import owe.production.CommodityState

trait HousingStructure
    extends BaseStructure
    with ProcessedUpdateMessages
    with ProcessedHousing
    with ProducedResources
    with ConsumedResources
    with ProcessedRisk
    with ProcessedTransition
    with GeneratedWalkers {

  import context.dispatcher

  override protected def behaviour: Behaviour = housing()

  final protected def housing(): Behaviour = {
    case ProcessEntityTick(map, structure: StructureData, messages) =>
      withUpdates(
        structure,
        Seq(
          withProcessedUpdateMessages(_: StructureData, messages),
          withProcessedHousing(_: StructureData),
          withProducedResources(_: StructureData),
          withConsumedResources(_: StructureData),
          withProcessedRisk(_: StructureData),
          withProcessedTransition(map, _: StructureData),
          withGeneratedWalkers(_: StructureData)
        )
      ).foreach { updatedData: StructureData =>
        CommodityCalculations
          .consumption(structure)
          .foreach(UpdateExchange.State(_, CommodityState.Used))

        CommodityCalculations
          .requiredCommodities(structure)
          .foreach(UpdateExchange.Stats.requiredCommodities(structure.properties.id, _))

        self ! Become(housing, updatedData)
      }
  }
}
