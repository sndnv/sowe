package owe.map.ops

import akka.actor.ActorLogging

trait Ops extends AvailabilityOps with PathingOps with EntityOps with QueryOps with ForwardingOps with TickOps {
  _: ActorLogging =>
}
