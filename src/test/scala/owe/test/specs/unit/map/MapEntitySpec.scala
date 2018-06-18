package owe.test.specs.unit.map

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.scalatest.Outcome
import owe.EntityDesirability
import owe.Tagging._
import owe.entities.Entity
import owe.entities.active.{Resource, Structure, Walker}
import owe.entities.passive.{Doodad, Road, Roadblock}
import owe.map.MapEntity
import owe.map.grid.Point
import owe.test.specs.unit.UnitSpec

class MapEntitySpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  private implicit val testMapEntityActorSystem: ActorSystem = ActorSystem("testMapEntityActorSystem")

  "A MapEntity" should "return the correct entity type" in { _ =>
    val entity = MapEntity(
      entityRef = TestProbe().ref.tag[Doodad.ActorRefTag]: Doodad.PassiveEntityActorRef,
      parentCell = Point(0, 0),
      size = Entity.Size(1, 1),
      desirability = EntityDesirability.Min
    )

    entity.entityType should be(Entity.Type.Doodad)

    entity
      .copy(
        entityRef = TestProbe().ref.tag[Road.ActorRefTag]: Road.PassiveEntityActorRef
      )
      .entityType should be(Entity.Type.Road)

    entity
      .copy(
        entityRef = TestProbe().ref.tag[Roadblock.ActorRefTag]: Roadblock.PassiveEntityActorRef
      )
      .entityType should be(Entity.Type.Roadblock)

    entity
      .copy(
        entityRef = TestProbe().ref.tag[Resource.ActorRefTag]: Resource.ActiveEntityActorRef
      )
      .entityType should be(Entity.Type.Resource)

    entity
      .copy(
        entityRef = TestProbe().ref.tag[Structure.ActorRefTag]: Structure.ActiveEntityActorRef
      )
      .entityType should be(Entity.Type.Structure)

    entity
      .copy(
        entityRef = TestProbe().ref.tag[Walker.ActorRefTag]: Walker.ActiveEntityActorRef
      )
      .entityType should be(Entity.Type.Walker)
  }

  it should "replace its parent cell" in { _ =>
    val entity = MapEntity(
      entityRef = TestProbe().ref.tag[Doodad.ActorRefTag]: Doodad.PassiveEntityActorRef,
      parentCell = Point(0, 0),
      size = Entity.Size(1, 1),
      desirability = EntityDesirability.Min
    )

    entity.parentCell should be(Point(0, 0))

    entity
      .copy(
        parentCell = Point(5, 12)
      )
      .parentCell should be(Point(5, 12))
  }
}
