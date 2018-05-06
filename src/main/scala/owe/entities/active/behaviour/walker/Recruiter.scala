package owe.entities.active.behaviour.walker

import owe.entities.ActiveEntity.{StructureData, WalkerData}
import owe.entities.active.Structure.{HousingState, NoHousing}
import owe.entities.active.Walker.MovementMode
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.entities.active.{Distance, Walker}

import scala.concurrent.Future

trait Recruiter extends BaseWalker {
  protected def recruitmentRadius: Distance

  import context.dispatcher

  final override protected def behaviour: Behaviour = roaming(DoRepeatableOperation(recruit, continueRoaming))

  private def recruit(walker: WalkerData): Future[Walker.State] =
    getNeighboursData(walker.properties.id, recruitmentRadius)
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
}
