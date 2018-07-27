package owe.entities.active.behaviour.structure.farming

import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor._
import owe.entities.active.Structure.CommoditiesState
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.structure.BaseStructure.Become
import owe.entities.active.behaviour.structure.transformations.{
  GeneratedWalkers,
  ProcessedFarming,
  ProcessedRisk,
  ProcessedUpdateMessages
}
import owe.entities.active.behaviour.structure.{BaseStructure, CommodityCalculations}
import owe.production.Commodity

trait FarmingStructure
    extends BaseStructure
    with ProcessedUpdateMessages
    with ProcessedFarming
    with ProcessedRisk
    with GeneratedWalkers {

  import context.dispatcher

  override protected def behaviour: Behaviour = farming()

  final protected def farming(): Behaviour = {
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
      log.debug("Processing behaviour tick as [farming] with data [{}]", structure)

      withUpdates(
        structure,
        Seq(
          withProcessedFarming(map, _: StructureData),
          withProcessedRisk(_: StructureData),
          withGeneratedWalkers(_: StructureData)
        )
      ).foreach { updatedData: StructureData =>
        CommodityCalculations
          .production(structure)
          .foreach(UpdateExchange.State(_, Commodity.State.Produced))

        (structure.state.commodities, updatedData.state.commodities) match {
          case (CommoditiesState(current, _), CommoditiesState(updated, _)) =>
            UpdateExchange.Stats.availableCommodities(structure.id, current, updated)

          case _ => //do nothing
        }

        self ! Become(() => farming(), updatedData)
      }
  }
}
