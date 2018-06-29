package owe.map.ops

import akka.Done
import akka.actor.ActorLogging
import akka.pattern.ask
import akka.util.Timeout
import owe.entities.ActiveEntity.{ActiveEntityActorRef, AddEntityMessage}
import owe.entities.Entity.EntityActorRef
import owe.entities.{ActiveEntity, Entity, PassiveEntity}
import owe.map.Cell.{CellActorRef, GetEntity}
import owe.map.MapEntity
import owe.map.grid.{Grid, Point}
import scala.concurrent.{ExecutionContext, Future}

import owe.entities.PassiveEntity.PassiveEntityActorRef

trait ForwardingOps { _: ActorLogging =>

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def forwardEntityMessage(
    grid: Grid[CellActorRef],
    entities: Map[EntityActorRef, Point],
    entityID: EntityActorRef,
    message: Entity.Message
  ): Future[Done] = {
    log.debug("Forwarding message [{}] to entity with ID [{}]", message, entityID)

    val result = for {
      parentCell <- entities.get(entityID)
      mapCell <- grid.get(parentCell)
    } yield {
      (mapCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].flatMap {
        case Some(mapEntity) =>
          mapEntity.entityRef match {
            case activeEntity: ActiveEntityActorRef =>
              activeEntity ! AddEntityMessage(message)
              Future.successful(Done)

            case _: PassiveEntityActorRef =>
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
