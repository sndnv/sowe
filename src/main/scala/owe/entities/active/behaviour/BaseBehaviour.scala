package owe.entities.active.behaviour

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.util.Timeout
import owe.entities.ActiveEntity._
import owe.entities.Entity
import scala.concurrent.duration._

import akka.actor.Scheduler

abstract class BaseBehaviour {

  type Behaviour = Behavior[EntityBehaviourMessage]

  def setup()(implicit ec: ExecutionContext): Behaviour = Behaviors.setup { ctx =>
    ctx.executionContext

    // TODO - + setup

    base.orElse(behaviour)
  }

  protected def base: Behaviour

  protected def behaviour(implicit ec: ExecutionContext): Behaviour

  protected implicit val scheduler: Scheduler = ??? // TODO
  protected implicit val timeout: Timeout = 1.second // TODO
  private[behaviour] implicit val parentEntity: ActorRef[EntityMessage] = ??? // TODO - context.parent.tag[P]

  private[behaviour] def withUpdates[D <: ActiveEntityData: ClassTag, S <: Entity.State](
    entity: D,
    updates: Seq[D => S]
  )(implicit ec: ExecutionContext): Future[D] =
    withAsyncUpdates(
      entity,
      updates.map { update => data: D =>
        Future.successful(update(data))
      }
    )

  private[behaviour] def withAsyncUpdates[D <: ActiveEntityData: ClassTag, S <: Entity.State](
    entity: D,
    updates: Seq[D => Future[S]]
  )(implicit ec: ExecutionContext): Future[D] = {
    @tailrec
    def applyUpdates(result: Future[S], remaining: Seq[D => Future[S]]): Future[S] =
      remaining match {
        case nextUpdate :: remainingUpdates =>
          applyUpdates(
            result.flatMap { updatedState =>
              entity.withState(updatedState) match {
                case update: D => nextUpdate(update)
                case data      => Future.failed(new IllegalStateException(s"Unexpected data encountered: [$data]"))
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
            case data      => Future.failed(new IllegalStateException(s"Unexpected data encountered: [$data]"))
          }
        }

      case _ => Future.successful(entity)
    }
  }
}
