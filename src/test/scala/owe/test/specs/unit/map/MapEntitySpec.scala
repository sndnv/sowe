package owe.test.specs.unit.map

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.{ActiveEntity, Entity}
import owe.entities.Entity.Desirability
import owe.entities.active.{Resource, Structure, Walker}
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.behaviour.resource.BaseResource
import owe.entities.active.behaviour.structure.BaseStructure
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.passive.{Doodad, Road, Roadblock}
import owe.entities.passive.Doodad.DoodadRef
import owe.entities.passive.Road.RoadRef
import owe.entities.passive.Roadblock.RoadblockRef
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
      entityRef = DoodadRef(TestProbe().ref),
      parentCell = Point(0, 0),
      spec = new Doodad
    )

    entity.entityType should be(Entity.Type.Doodad)

    entity
      .copy(
        entityRef = RoadRef(TestProbe().ref)
      )
      .entityType should be(Entity.Type.Road)

    entity
      .copy(
        entityRef = RoadblockRef(TestProbe().ref)
      )
      .entityType should be(Entity.Type.Roadblock)

    entity
      .copy(
        entityRef = ResourceRef(TestProbe().ref)
      )
      .entityType should be(Entity.Type.Resource)

    entity
      .copy(
        entityRef = StructureRef(TestProbe().ref)
      )
      .entityType should be(Entity.Type.Structure)

    entity
      .copy(
        entityRef = WalkerRef(TestProbe().ref)
      )
      .entityType should be(Entity.Type.Walker)
  }

  it should "replace its parent cell" in { _ =>
    val entity = MapEntity(
      entityRef = DoodadRef(TestProbe().ref),
      parentCell = Point(0, 0),
      spec = new Doodad
    )

    entity.parentCell should be(Point(0, 0))

    entity
      .copy(
        parentCell = Point(5, 12)
      )
      .parentCell should be(Point(5, 12))
  }

  it should "create correct entities" in { _ =>
    MapEntity(
      actorRef = TestProbe().ref,
      parentCell = Point(0, 0),
      spec = new Doodad
    ).entityRef shouldBe a[DoodadRef]

    MapEntity(
      actorRef = TestProbe().ref,
      parentCell = Point(0, 0),
      spec = new Road
    ).entityRef shouldBe a[RoadRef]

    MapEntity(
      actorRef = TestProbe().ref,
      parentCell = Point(0, 0),
      spec = new Roadblock
    ).entityRef shouldBe a[RoadblockRef]

    MapEntity(
      actorRef = TestProbe().ref,
      parentCell = Point(0, 0),
      spec = new Resource {
        override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
        override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, Effect)] = ???
        override protected def createBehaviour(): BaseResource = ???
      }
    ).entityRef shouldBe a[ResourceRef]

    MapEntity(
      actorRef = TestProbe().ref,
      parentCell = Point(0, 0),
      spec = new Structure {
        override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
        override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, Effect)] = ???
        override protected def createBehaviour(): BaseStructure = ???
        override def `size`: Entity.Size = ???
        override def `desirability`: Desirability = ???
      }
    ).entityRef shouldBe a[StructureRef]

    MapEntity(
      actorRef = TestProbe().ref,
      parentCell = Point(0, 0),
      spec = new Walker {
        override def spawnLocation: Walker.SpawnLocation = ???
        override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = ???
        override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, Effect)] = ???
        override protected def createBehaviour(): BaseWalker = ???
      }
    ).entityRef shouldBe a[WalkerRef]
  }
}
