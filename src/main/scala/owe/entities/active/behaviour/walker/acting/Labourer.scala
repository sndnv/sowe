package owe.entities.active.behaviour.walker.acting

import owe.entities.ActiveEntity.WalkerData
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.CommoditiesState
import owe.entities.active.behaviour.walker.BaseWalker._

trait Labourer extends ActingWalker {
  protected def actions: Seq[Action]

  override protected def behaviour: Behaviour = acting(actions)

  final protected def gatherResources(source: ResourceRef, target: StructureRef): Seq[Action] = Seq[Action](
    GoToEntity(source),
    DoOperation(gather(_, source, target)),
    GoHome(),
    DoRepeatableOperation(unload(_, target), unloadNotComplete)
  )

  private def unloadNotComplete(walker: WalkerData): Boolean =
    walker.state.commodities match {
      case CommoditiesState(available, _) => available.nonEmpty
      case _                              => false // has no commodities
    }
}
