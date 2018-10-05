package owe.test.specs.unit.entities.active.behaviour

import akka.actor.{Actor, ActorRef, Props}
import akka.testkit.TestProbe
import owe.entities.ActiveEntity._
import owe.entities.ActiveEntityActor._
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.{CommoditiesState, StructureRef}
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.attributes.Distance
import owe.entities.passive.Doodad
import owe.entities.passive.Doodad.DoodadRef
import owe.map.GameMap._
import owe.map.MapEntity
import owe.map.grid.Point
import owe.production.Commodity
import owe.test.specs.unit.entities.active.behaviour.WalkerParentEntity.{ForwardToChild, MockDestroySelf}
import owe.test.specs.unit.entities.definitions.active.walkers.Archer

import scala.collection.immutable.Queue

class WalkerParentEntity(
  ref: ActorRef,
  childProps: Props
) extends Actor {
  private val child = context.actorOf(childProps)

  import context.system

  override def receive: Receive = {
    case apply: ApplyInstructions   => child ! apply
    case apply: ApplyMessages       => child ! apply
    case tick: ProcessBehaviourTick => child ! tick
    case create: CreateBehaviour    => child ! create
    case MockDestroySelf(data)      => child ! DestroyBehaviour(data)
    case ForwardToChild(message)    => child ! message

    case ForwardMessage(GetAdjacentPoint(_, _)) =>
      sender ! None

    case ForwardMessage(GetEntity(entityID)) =>
      val result: Data = entityID match {
        case entityRef: WalkerRef =>
          WalkerData(
            Fixtures.Walker.properties,
            Fixtures.Walker.state,
            Fixtures.Walker.modifiers,
            entityRef
          )

        case entityRef: StructureRef =>
          StructureData(
            Fixtures.Structure.Producing.properties,
            Fixtures.Structure.Producing.state.copy(
              commodities = CommoditiesState(
                available = Map(Commodity("TestCommodity") -> Commodity.Amount(100)),
                limits = Map(Commodity("TestCommodity") -> Commodity.Amount(175))
              )
            ),
            Fixtures.Structure.Producing.modifiers,
            entityRef
          )

        case entityRef: ResourceRef =>
          ResourceData(
            Fixtures.Resource.properties,
            Fixtures.Resource.state,
            Fixtures.Resource.modifiers,
            entityRef
          )
      }

      sender ! result

    case ForwardMessage(GetNeighbours(_, radius)) =>
      val result: Seq[(ActiveEntityRef, Data)] = if (radius > Distance(1)) {
        val testWalkerRef1 = WalkerRef(TestProbe().ref)
        val testWalkerRef2 = WalkerRef(TestProbe().ref)

        Seq(
          (testWalkerRef1,
           WalkerData(
             Fixtures.Walker.properties,
             Fixtures.Walker.state,
             Fixtures.Walker.modifiers,
             testWalkerRef1
           )),
          (testWalkerRef2,
           WalkerData(
             Fixtures.Walker.properties,
             Fixtures.Walker.state,
             Fixtures.Walker.modifiers,
             testWalkerRef2
           ))
        )
      } else {
        Seq.empty
      }

      sender ! result

    case ForwardMessage(GetEntities(point)) =>
      val result: Seq[(MapEntity, Option[Data])] = if (point == Point(0, 0)) {
        val walkerRef = WalkerRef(TestProbe().ref)

        val walkerEntity = MapEntity(
          entityRef = walkerRef,
          parentCell = point,
          spec = new Archer
        )

        val walkerData = WalkerData(
          Fixtures.Walker.properties,
          Fixtures.Walker.state,
          Fixtures.Walker.modifiers,
          walkerRef
        )

        val doodadEntity = MapEntity(
          entityRef = DoodadRef(TestProbe().ref),
          parentCell = point,
          spec = new Doodad
        )

        Seq(
          (doodadEntity, None),
          (walkerEntity, Some(walkerData))
        )
      } else {
        Seq.empty
      }

      sender ! result

    case ForwardMessage(GetAdvancePath(_, destination)) =>
      val defaultPath = Queue[Point](
        (1, 0),
        (2, 0),
        (2, 1),
        (1, 1)
      )

      val result: Queue[Point] = destination match {
        case Point(0, 0) => Queue.empty
        case Point(1, 1) => defaultPath
        case _           => defaultPath :+ destination
      }

      sender ! result

    case ForwardMessage(GetRoamingPath(_, length)) =>
      val result: Queue[Point] = if (length >= Distance(3)) {
        Queue(
          (0, 1),
          (0, 2),
          (1, 2)
        )
      } else {
        Queue.empty
      }

      sender ! result

    case message => ref.forward(message)
  }
}

object WalkerParentEntity {
  case class ForwardToChild(message: Any)
  case class MockDestroySelf(data: WalkerData)

  def props(ref: ActorRef, childProps: Props) = Props(classOf[WalkerParentEntity], ref, childProps)
}
