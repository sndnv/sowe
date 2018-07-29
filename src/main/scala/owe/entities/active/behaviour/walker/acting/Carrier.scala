package owe.entities.active.behaviour.walker.acting

import owe.entities.ActiveEntity.WalkerData
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.CommoditiesState
import owe.entities.active.behaviour.walker.BaseWalker._

trait Carrier extends ActingWalker {
  protected def actions: Seq[Action]
  protected def canReturnCommodities: Boolean

  final protected def retrieve(from: StructureRef, to: StructureRef): Seq[Action] =
    if (canReturnCommodities) {
      Seq[Action](
        GoToEntity(from),
        DoOperation(load(_, from, to)),
        GoToEntity(to),
        DoOperation(unload(_, to)),
        GoToEntity(from),
        DoRepeatableOperation(unload(_, from), hasCommodities),
        GoHome()
      )
    } else {
      Seq[Action](
        GoToEntity(from),
        DoOperation(load(_, from, to)),
        GoHome(),
        DoRepeatableOperation(unload(_, to), hasCommodities)
      )
    }

  final protected def deliver(from: StructureRef, to: StructureRef): Seq[Action] =
    if (canReturnCommodities) {
      Seq[Action](
        DoOperation(load(_, from, to)),
        GoToEntity(to),
        DoOperation(unload(_, to)),
        GoHome(),
        DoRepeatableOperation(unload(_, from), hasCommodities)
      )
    } else {
      Seq[Action](
        DoOperation(load(_, from, to)),
        GoToEntity(to),
        DoRepeatableOperation(unload(_, to), hasCommodities),
        GoHome()
      )
    }

  private def hasCommodities(walker: WalkerData): Boolean =
    walker.state.commodities match {
      case CommoditiesState(available, _) => available.nonEmpty
      case _                              => false // has no commodities
    }

  final override protected def behaviour: Behaviour = acting(actions)
}
