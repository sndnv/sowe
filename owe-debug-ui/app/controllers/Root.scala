package controllers

import javax.inject._

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.util.Timeout
import game.GameData
import game.entities.JsonFormatters
import game.entities.resources.TeaPlant
import game.entities.structures.{Firehouse, House, StorageBuilding}
import game.entities.walkers.Courier
import game.map.{EntityObserver, EventObserver, ExchangeObserver}
import owe.entities.ActiveEntity.{ActiveEntityRef, StructureData}
import owe.entities.Entity.EntityRef
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Doodad.DoodadRef
import owe.entities.passive.Road.RoadRef
import owe.entities.passive.Roadblock.RoadblockRef
import owe.entities.passive.{Doodad, Road, Roadblock}
import owe.events.Event.EntityEvent
import owe.map.grid.Point
import play.api.libs.json._
import play.api.libs.streams.ActorFlow
import play.api.mvc.WebSocket.MessageFlowTransformer
import play.api.mvc._

@Singleton
class Root @Inject()(
  game: GameData,
  cc: ControllerComponents
)(implicit system: ActorSystem, assetsFinder: AssetsFinder, mat: Materializer)
    extends AbstractController(cc) {
  private implicit val mapTimeout: Timeout = 3.seconds
  private implicit val exchangeCollectionInterval: FiniteDuration = 1.second

  private val entityRefRegex = """^(.+)\(Actor\[akka:\/\/.+\/user\/(.+)\/(.+)\]\)""".r

  import ExchangeObserver.ExchangeEvent._
  import JsonFormatters._
  import system.dispatcher

  private implicit val entitiesMessageFlowTransformer: MessageFlowTransformer[String, EntityEvent] =
    MessageFlowTransformer.jsonMessageFlowTransformer[String, EntityEvent]

  private implicit val eventsMessageFlowTransformer: MessageFlowTransformer[String, EventObserver.SerializableEvent] =
    MessageFlowTransformer.jsonMessageFlowTransformer[String, EventObserver.SerializableEvent]

  private implicit val exchangeMessageFlowTransformer: MessageFlowTransformer[String, ExchangeObserver.ExchangeEvent] =
    MessageFlowTransformer.jsonMessageFlowTransformer[String, ExchangeObserver.ExchangeEvent]

  private val storage_01: StructureRef = game.map
    .createEntity(
      new StorageBuilding(home = (1, 1), walkerGenerators = Map.empty),
      cell = (1, 1)
    ) match {
    case ref: StructureRef => ref
  }

  private def courierGen: StructureData => Option[Walker] =
    (parent: StructureData) => {
      Some(
        new Courier(
          parent.id,
          (9, 9),
          Courier.Parameters(source = parent.id, target = storage_01)
        )
      )
    }

  game.map
    .createEntity(
      new StorageBuilding(
        home = (27, 27),
        walkerGenerators = Map(
          "courier" -> courierGen
        )
      ),
      cell = (27, 27)
    )

  game.map
    .createEntity(
      new StorageBuilding(
        home = (10, 10),
        walkerGenerators = Map.empty
      ),
      cell = (10, 10)
    )

  game.map.createEntity(new TeaPlant((16, 16)), cell = (16, 16))

  Root.roadCells.foreach { cell =>
    game.map.createEntity(new Road, cell)
  }

  def index = Action { implicit request =>
    val grid = game.map.grid.map { cell =>
      val entities = cell.entities.values.toSeq

      (
        Json.toJson(cell.state),
        cell.`type`,
        Json.toJsObject(entities.map(e => (e.entityRef.toString, e)).toMap)
      )
    }

    val structureSizes: JsValue = Json.toJson(
      Map(
        "Firehouse" -> Firehouse.size,
        "House" -> House.size,
        "StorageBuilding" -> StorageBuilding.size
      )
    )

    Ok(views.html.index(structureSizes, grid.indexed().rows))
  }

  def generator = Action { implicit request =>
    Ok(views.html.generator(game.map.grid.map(_.`type`).indexed().rows))
  }

  def constructEntity = Action { implicit request =>
    Root.entityForm.bindFromRequest.fold(
      form => {
        BadRequest(form.errors.map(_.message).mkString)
      },
      entityData => {
        val home = Point(entityData.x, entityData.y)
        val entity = entityData.entityType match {
          case "Firehouse"       => new Firehouse(home)
          case "House"           => new House(home)
          case "StorageBuilding" => new StorageBuilding(home, walkerGenerators = Map("courier" -> courierGen))
          case "TeaPlant"        => new TeaPlant(home)
          case "Road"            => new Road()
          case "Roadblock"       => new Roadblock()
          case "Doodad"          => new Doodad()
        }

        val entityRef = game.map.createEntity(entity, home)

        Ok(entityRef.toString)
      }
    )
  }

  def destroyEntity(entityID: String): Action[AnyContent] = Action.async {
    entityStringToID(entityID).map { refOpt =>
      refOpt
        .map { ref =>
          game.map.destroyEntity(ref)
          Ok
        }
        .getOrElse(NotFound)
    }
  }

  def getEntity(entityID: String): Action[AnyContent] = Action.async {
    entityStringToID(entityID).flatMap { refOpt =>
      refOpt
        .map {
          case ref: ActiveEntityRef =>
            game.map
              .getEntityAsync(ref)
              .map { entityData =>
                Ok(Json.toJson(entityData))
              }
        }
        .getOrElse(Future.successful(NotFound))
    }
  }

  def events: WebSocket = WebSocket.accept[String, EventObserver.SerializableEvent] { _ =>
    ActorFlow.actorRef { out =>
      EventObserver.props(game.tracker, out)
    }
  }

  def entities: WebSocket = WebSocket.accept[String, EntityEvent] { _ =>
    ActorFlow.actorRef { out =>
      EntityObserver.props(game.tracker, out)
    }
  }

  def exchange: WebSocket = WebSocket.accept[String, ExchangeObserver.ExchangeEvent] { _ =>
    ActorFlow.actorRef { out =>
      ExchangeObserver.props(game.exchange, out, exchangeCollectionInterval)
    }
  }

  private def entityStringToID(entityID: String): Future[Option[EntityRef]] =
    entityID match {
      case entityRefRegex(entityRefType, entityParent, entity) =>
        system.actorSelection(s"user/$entityParent/$entity").resolveOne().map { ref =>
          val entityRef: EntityRef = entityRefType match {
            case "ResourceRef"  => ResourceRef(ref)
            case "StructureRef" => StructureRef(ref)
            case "WalkerRef"    => WalkerRef(ref)
            case "DoodadRef"    => DoodadRef(ref)
            case "RoadRef"      => RoadRef(ref)
            case "RoadblockRef" => RoadblockRef(ref)
          }

          Some(entityRef)
        }

      case _ =>
        Future.successful(None)
    }
}

object Root {
  import play.api.data.Forms._
  import play.api.data._

  case class NewEntity(x: Int, y: Int, entityType: String)

  private val entityForm = Form(
    mapping(
      "x" -> number,
      "y" -> number,
      "entityType" -> text
    )(NewEntity.apply)(NewEntity.unapply)
  )

  private val roadCells = Seq(
    Point(0, 0),
    Point(1, 0),
    Point(2, 0),
    Point(3, 0),
    Point(4, 0),
    Point(5, 0),
    Point(5, 1),
    Point(5, 2),
    Point(5, 3),
    Point(6, 3),
    Point(7, 3),
    Point(8, 3),
    Point(9, 3),
    Point(9, 4),
    Point(9, 5),
    Point(9, 6),
    Point(9, 7),
    Point(9, 8),
    Point(9, 9),
    Point(9, 10),
    Point(9, 11),
    Point(9, 12),
    Point(9, 15),
    Point(9, 16),
    Point(9, 14),
    Point(9, 13),
    Point(9, 17),
    Point(9, 18),
    Point(9, 19),
    Point(0, 20),
    Point(1, 20),
    Point(2, 20),
    Point(3, 20),
    Point(4, 20),
    Point(5, 20),
    Point(6, 20),
    Point(7, 20),
    Point(8, 20),
    Point(9, 20)
  )
}
