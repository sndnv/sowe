package owe.entities.active.behaviour

import akka.actor.{Actor, ActorLogging}
import akka.util.Timeout
import owe.entities.ActiveEntity.{ActiveEntityRef, Data}
import owe.entities.Entity

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.reflect.ClassTag

trait BaseBehaviour extends Actor with ActorLogging {
  import context.dispatcher

  type Behaviour = Receive

  protected implicit val timeout: Timeout = 1.second

  protected def base: Behaviour

  protected def behaviour: Behaviour

  private[behaviour] implicit val parentEntity: ActiveEntityRef

  final override def receive: Receive = base.orElse(behaviour)

  private[owe] def withUpdates[D <: Data: ClassTag, S <: Entity.State](
    entity: D,
    updates: Seq[D => S]
  ): Future[D] =
    withAsyncUpdates(
      entity,
      updates.map { update => data: D =>
        Future.successful(update(data))
      }
    )

  private[owe] def withAsyncUpdates[D <: Data: ClassTag, S <: Entity.State](
    entity: D,
    updates: Seq[D => Future[S]]
  ): Future[D] = {
    @tailrec
    def applyUpdates(result: Future[S], remaining: Seq[D => Future[S]]): Future[S] =
      remaining match {
        case nextUpdate :: remainingUpdates =>
          applyUpdates(
            result.flatMap { updatedState =>
              entity.withState(updatedState) match {
                case update: D => nextUpdate(update)
              }
            },
            remainingUpdates
          )

        case _ =>
          result
      }

    updates match {
      case head :: tail =>
        applyUpdates(head(entity), tail).flatMap { updatedState =>
          entity.withState(updatedState) match {
            case update: D => Future.successful(update)
          }
        }

      case _ => Future.successful(entity)
    }
  }
}
