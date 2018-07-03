package owe.entities.active.behaviour.walker.roaming

import owe.entities.ActiveEntity.{ForwardMessage, StructureData, WalkerData}
import owe.entities.active.Structure.{HousingState, NoHousing, StructureRef}
import owe.entities.active.Walker.MovementMode
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.entities.active.{Distance, Walker}
import owe.map.GameMap.LabourFound

import scala.concurrent.Future

trait Recruiter extends BaseWalker {
  protected def recruitmentRadius: Distance

  import context.dispatcher

  final override protected def behaviour: Behaviour = roaming(DoRepeatableOperation(recruit, continueRoaming))

  private def recruit(walker: WalkerData): Future[Walker.State] =
    getNeighboursData(walker.id, recruitmentRadius)
      .map { neighbours =>
        neighbours.exists {
          case (_, structure: StructureData) =>
            structure.state.housing match {
              case housing: HousingState => housing.occupants > 0
              case NoHousing             => false
            }

          case _ => false
        }
      }
      .map { labourFound =>
        if (labourFound) {
          walker.properties.parent.foreach(sendLabourNotification)
          walker.state.copy(mode = MovementMode.Returning)
        } else {
          walker.state
        }
      }

  private def continueRoaming(walker: WalkerData): Boolean =
    walker.state.mode == MovementMode.Roaming

  private def sendLabourNotification(parentID: StructureRef): Unit =
    parentEntity ! ForwardMessage(LabourFound(parentID))
}
