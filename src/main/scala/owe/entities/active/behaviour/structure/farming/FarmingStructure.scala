package owe.entities.active.behaviour.structure.farming

import scala.concurrent.ExecutionContext

import akka.actor.typed.scaladsl.Behaviors
import owe.entities.ActiveEntity.{ProcessEntityTick, StructureData}
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
import owe.production.CommodityState

trait FarmingStructure
    extends BaseStructure
    with ProcessedUpdateMessages
    with ProcessedFarming
    with ProcessedRisk
    with GeneratedWalkers {

  override protected def behaviour(implicit ec: ExecutionContext): Behaviour = farming()

  final protected def farming(): Behaviour = Behaviors.receive { (ctx, msg) =>
    import ctx.executionContext

    msg match {
      case ProcessEntityTick(map, structure: StructureData, messages) =>
        withUpdates(
          structure,
          Seq(
            withProcessedUpdateMessages(_: StructureData, messages),
            withProcessedFarming(map, _: StructureData),
            withProcessedRisk(_: StructureData),
            withGeneratedWalkers(_: StructureData)
          )
        ).foreach { updatedData: StructureData =>
          CommodityCalculations
            .production(structure)
            .foreach(UpdateExchange.State(_, CommodityState.Produced))

          (structure.state.commodities, updatedData.state.commodities) match {
            case (CommoditiesState(current, _), CommoditiesState(updated, _)) =>
              UpdateExchange.Stats.availableCommodities(structure.id, current, updated)

            case _ => //do nothing
          }

          ctx.self ! Become(() => farming(), updatedData)
        }
    }

    Behaviors.same // TODO
  }
}
