package owe.map.ops

import akka.pattern.ask
import akka.util.Timeout
import owe.entities.ActiveEntity.ActiveEntityRef
import owe.entities.ActiveEntityActor.AddEntityMessage
import owe.entities.Entity
import owe.entities.Entity.EntityRef
import owe.entities.PassiveEntity.PassiveEntityRef
import owe.events.Event
import owe.map.Cell.{CellActorRef, GetEntity}
import owe.map.MapEntity
import owe.map.grid.{Grid, Point}

import scala.concurrent.{ExecutionContext, Future}

trait ForwardingOps {

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def forwardEntityMessage(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef,
    message: Entity.Message
  ): Future[Event] = {
    val result = for {
      parentCell <- entities
        .get(entityID)
        .toRight(Event(Event.System.CellsUnavailable, cell = None)): Either[Event, Point]
      mapCell <- grid
        .get(parentCell)
        .toRight(Event(Event.System.CellOutOfBounds, Some(parentCell))): Either[Event, CellActorRef]
    } yield {
      (mapCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].map {
        case Some(mapEntity) =>
          mapEntity.entityRef match {
            case activeEntity: ActiveEntityRef =>
              activeEntity ! AddEntityMessage(message)
              Event(Event.System.MessageForwarded, cell = Some(parentCell))

            case _: PassiveEntityRef =>
              Event(Event.System.UnexpectedEntityFound, Some(parentCell))
          }

        case None =>
          Event(Event.System.EntityMissing, Some(parentCell))
      }
    }

    result match {
      case Left(event)        => Future.successful(event)
      case Right(futureEvent) => futureEvent
    }
  }
}
