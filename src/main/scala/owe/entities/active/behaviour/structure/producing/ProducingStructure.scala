package owe.entities.active.behaviour.structure.producing

import owe.entities.ActiveEntity.{ProcessEntityTick, StructureData}
import owe.entities.active.Structure.CommoditiesState
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.structure.BaseStructure.Become
import owe.entities.active.behaviour.structure.transformations._
import owe.entities.active.behaviour.structure.{BaseStructure, CommodityCalculations}
import owe.production.CommodityState

trait ProducingStructure
    extends BaseStructure
    with ProcessedUpdateMessages
    with ProducedResources
    with ConsumedResources
    with ProcessedRisk
    with ProcessedTransition
    with GeneratedWalkers {

  import context.dispatcher

  override protected def behaviour: Behaviour = producing()

  final protected def producing(): Behaviour = {
    case ProcessEntityTick(map, structure: StructureData, messages) =>
      withUpdates(
        structure,
        Seq(
          withProcessedUpdateMessages(_: StructureData, messages),
          withProducedResources(_: StructureData),
          withConsumedResources(_: StructureData),
          withProcessedRisk(_: StructureData),
          withProcessedTransition(map, _: StructureData),
          withGeneratedWalkers(_: StructureData)
        )
      ).foreach { updatedData: StructureData =>
        CommodityCalculations
          .production(structure)
          .foreach(UpdateExchange.State(_, CommodityState.Produced))

        CommodityCalculations
          .consumption(structure)
          .foreach(UpdateExchange.State(_, CommodityState.Used))

        CommodityCalculations
          .requiredCommodities(structure)
          .foreach(UpdateExchange.Stats.requiredCommodities(structure.properties.id, _))

        (structure.state.commodities, updatedData.state.commodities) match {
          case (CommoditiesState(current, _), CommoditiesState(updated, _)) =>
            UpdateExchange.Stats.availableCommodities(structure.properties.id, current, updated)

          case _ => //do nothing
        }

        self ! Become(producing, updatedData)
      }
  }
}
