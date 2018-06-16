package owe.map.ops

import akka.Done
import akka.actor.ActorLogging
import akka.pattern.ask
import akka.util.Timeout
import owe.EntityID
import owe.entities.ActiveEntity.AddEntityMessage
import owe.entities.Entity
import owe.map.Cell.{CellActorRef, GetEntity}
import owe.map.grid.{Grid, Point}
import owe.map.{ActiveMapEntity, MapEntity, PassiveMapEntity}

import scala.concurrent.{ExecutionContext, Future}

trait ForwardingOps { _: ActorLogging =>

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def forwardEntityMessage(
    grid: Grid[CellActorRef],
    entities: Map[EntityID, Point],
    entityID: EntityID,
    message: Entity.Message
  ): Future[Done] = {
    log.debug("Forwarding message [{}] to entity with ID [{}]", message, entityID)

    val result = for {
      parentCell <- entities.get(entityID)
      mapCell <- grid.get(parentCell)
    } yield {
      (mapCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].flatMap {
        case Some(mapEntity) =>
          mapEntity match {
            case activeEntity: ActiveMapEntity =>
              activeEntity.entity ! AddEntityMessage(message)
              Future.successful(Done)

            case _: PassiveMapEntity =>
              val errorMessage = s"Can't forward message [$message] to passive entity with ID [$entityID]."
              log.error(errorMessage)
              Future.failed(new IllegalStateException(errorMessage))
          }

        case None =>
          val errorMessage =
            s"Failed to find entity with ID [$entityID] in cell [$parentCell] while processing message [$message]."
          log.error(errorMessage)
          Future.failed(new IllegalStateException(errorMessage))
      }
    }

    result match {
      case Some(future) =>
        future
          .map { _ =>
            log.debug("Message [{}] forwarded to entity with ID [{}].", message, entityID)
            Done
          }
          .recover {
            case e =>
              log.error(e.getMessage)
              Done
          }

      case None =>
        log.error("Failed to find entity with ID [{}] while processing message [{}].", entityID, message)
        Future.successful(Done)
    }
  }
}
