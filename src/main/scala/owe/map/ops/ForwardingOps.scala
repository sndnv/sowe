package owe.map.ops

import akka.actor.{Actor, ActorRef}
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

import owe.events.Event.{CellEvent, SystemEvent}

trait ForwardingOps {

  protected implicit val actionTimeout: Timeout
  protected implicit val ec: ExecutionContext

  def forwardEntityMessage(
    grid: Grid[CellActorRef],
    entities: Map[EntityRef, Point],
    entityID: EntityRef,
    message: Entity.Message
  )(implicit sender: ActorRef = Actor.noSender): Future[Event] = {
    val result = for {
      parentCell <- entities
        .get(entityID)
        .toRight(SystemEvent(Event.Engine.EntityMissing)): Either[Event, Point]
      mapCell <- grid
        .get(parentCell)
        .toRight(CellEvent(Event.Engine.CellOutOfBounds, parentCell)): Either[Event, CellActorRef]
    } yield {
      (mapCell ? GetEntity(entityID)).mapTo[Option[MapEntity]].map {
        case Some(mapEntity) =>
          mapEntity.entityRef match {
            case activeEntity: ActiveEntityRef =>
              activeEntity ! AddEntityMessage(message)
              CellEvent(Event.Engine.MessageForwarded, targetCell = parentCell)

            case _: PassiveEntityRef =>
              CellEvent(Event.Engine.UnexpectedEntityFound, parentCell)
          }

        case None =>
          CellEvent(Event.Engine.EntityMissing, parentCell)
      }
    }

    result match {
      case Left(event)        => Future.successful(event)
      case Right(futureEvent) => futureEvent
    }
  }
}
