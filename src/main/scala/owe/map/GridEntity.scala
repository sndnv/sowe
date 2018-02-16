package owe.map

import akka.actor.ActorRef

case class GridEntity(entity: ActorRef, parent: Location)
