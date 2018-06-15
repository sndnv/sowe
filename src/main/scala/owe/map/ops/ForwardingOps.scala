package owe.map.ops

import akka.actor.ActorLogging
import owe.EntityID
import owe.entities.ActiveEntity.AddEntityMessage
import owe.entities.Entity
import owe.map.grid.{Grid, Point}
import owe.map.{ActiveMapEntity, MapCell, PassiveMapEntity}

trait ForwardingOps { _: ActorLogging =>
  def forwardEntityMessage(
    grid: Grid[MapCell],
    entities: Map[EntityID, Point],
    entityID: EntityID,
    message: Entity.Message
  ): Unit = {
    log.debug("Forwarding message [{}] to entity with ID [{}]", message, entityID)

    (for {
      parentCell <- entities.get(entityID)
      mapCell <- grid.get(parentCell)
      mapEntity <- mapCell.entities.get(entityID)
    } yield {
      mapEntity match {
        case activeEntity: ActiveMapEntity =>
          activeEntity.entity ! AddEntityMessage(message)

        case _: PassiveMapEntity =>
          log.error("Can't forward message [{}] to passive entity with ID [{}].", message, entityID)
      }
    }) match {
      case Some(_) => log.debug("Message [{}] forwarded to entity with ID [{}].", message, entityID)
      case None    => log.error("Failed to find entity with ID [{}] while processing message [{}].", entityID, message)
    }
  }
}
