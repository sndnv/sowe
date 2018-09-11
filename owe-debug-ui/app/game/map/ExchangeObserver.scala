package game.map

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Timers}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import owe.production.Exchange._
import play.api.libs.json._

import scala.concurrent.duration.FiniteDuration

class ExchangeObserver(
  exchange: ActorRef,
  out: ActorRef,
  collectionInterval: FiniteDuration
)(implicit timeout: Timeout)
    extends Actor
    with ActorLogging
    with Timers {
  import ExchangeObserver._
  import context.dispatcher

  timers.startPeriodicTimer(CollectionTimer, Collect, collectionInterval)

  override def receive: Receive = {
    case ExchangeObserver.Collect =>
      val commodities = (exchange ? GetExchangeCommodities()).mapTo[ExchangeCommodities]
      val stats = (exchange ? GetExchangeStats()).mapTo[ExchangeStats]
      val entities = (exchange ? GetExchangeEntities()).mapTo[ExchangeEntities]

      val result = for {
        commodities <- commodities
        stats <- stats
        entities <- entities
      } yield {
        ExchangeObserver.ExchangeEvent(
          commodities,
          stats,
          entities
        )
      }

      result.pipeTo(out)

    case message =>
      log.warning(s"[entity-observer / $self / $out] Received unexpected message: [$message]")
  }
}

object ExchangeObserver {

  private case object CollectionTimer
  private case object Collect

  case class ExchangeEvent(
    commodities: ExchangeCommodities,
    stats: ExchangeStats,
    entities: ExchangeEntities
  )

  object ExchangeEvent {
    import game.entities.JsonFormatters._
    implicit val eventFormat: Format[ExchangeEvent] = Json.format[ExchangeEvent]
  }

  def props(
    exchange: ActorRef,
    out: ActorRef,
    collectionInterval: FiniteDuration
  )(implicit timeout: Timeout): Props =
    Props(
      classOf[ExchangeObserver],
      exchange,
      out,
      collectionInterval,
      timeout
    )
}
