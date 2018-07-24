package owe.entities.active.behaviour.structure.housing

import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor.{ApplyMessages, MessagesApplied, ProcessBehaviourTick}
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.structure.BaseStructure.Become
import owe.entities.active.behaviour.structure.transformations._
import owe.entities.active.behaviour.structure.{BaseStructure, CommodityCalculations}
import owe.production.Commodity

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
    case ApplyMessages(structure: StructureData, messages) =>
      log.debug("Applying [{}] messages: [{}]", messages.size, messages)

      withUpdates(
        structure,
        Seq(
          withProcessedUpdateMessages(_: StructureData, messages)
        )
      ).foreach { updatedData =>
        parentEntity ! MessagesApplied(updatedData.state)
      }

    case ProcessBehaviourTick(map, structure: StructureData) =>
      log.debug("Processing behaviour tick as [housing] with data [{}]", structure)

      withUpdates(
        structure,
        Seq(
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
          .foreach(UpdateExchange.State(_, Commodity.State.Used))

        CommodityCalculations
          .requiredCommodities(updatedData)
          .foreach(UpdateExchange.Stats.requiredCommodities(structure.id, _))

        self ! Become(() => housing(), updatedData)
      }
  }
}
