package owe.entities.active.behaviour.structure.producing

import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor._
import owe.entities.active.Structure.CommoditiesState
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.structure.BaseStructure.Become
import owe.entities.active.behaviour.structure.transformations._
import owe.entities.active.behaviour.structure.{BaseStructure, CommodityCalculations}
import owe.production.Commodity

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
    case ApplyInstructions(_, instructions) =>
      log.debug("Applying [{}] instructions: [{}]", instructions.size, instructions)
      instructions.foreach { instruction =>
        log.warning("Instruction [{}] is not supported", instruction)
      }
      parentEntity ! InstructionsApplied()

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
      log.debug("Processing behaviour tick as [producing] with data [{}]", structure)

      withUpdates(
        structure,
        Seq(
          withProducedResources(_: StructureData),
          withConsumedResources(_: StructureData),
          withProcessedRisk(_: StructureData),
          withProcessedTransition(map, _: StructureData),
          withGeneratedWalkers(_: StructureData)
        )
      ).foreach { updatedData: StructureData =>
        CommodityCalculations
          .production(structure)
          .foreach(UpdateExchange.State(_, Commodity.State.Produced))

        CommodityCalculations
          .consumption(structure)
          .foreach(UpdateExchange.State(_, Commodity.State.Used))

        CommodityCalculations
          .requiredCommodities(updatedData)
          .foreach(UpdateExchange.Stats.requiredCommodities(structure.id, _))

        (structure.state.commodities, updatedData.state.commodities) match {
          case (CommoditiesState(current, _), CommoditiesState(updated, _)) =>
            UpdateExchange.Stats.availableCommodities(structure.id, current, updated)

          case _ => //do nothing
        }

        self ! Become(() => producing(), updatedData)
      }
  }
}
