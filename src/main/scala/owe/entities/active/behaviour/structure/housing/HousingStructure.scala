package owe.entities.active.behaviour.structure.housing

import akka.actor.typed.scaladsl.Behaviors
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

  override protected def behaviour: Behaviour = housing()

  final protected def housing(): Behaviour = Behaviors.receive { (ctx, msg) =>
    import ctx.executionContext

    msg match {
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
            .requiredCommodities(updatedData)
            .foreach(UpdateExchange.Stats.requiredCommodities(structure.id, _))

          ctx.self ! Become(() => housing(), updatedData)
        }
    }

    Behaviors.same // TODO
  }
}
