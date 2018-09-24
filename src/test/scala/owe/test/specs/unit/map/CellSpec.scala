package owe.test.specs.unit.map

import akka.actor.ActorRef
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.entities.Entity
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.{Doodad, Road}
import owe.entities.passive.Doodad.DoodadRef
import owe.entities.passive.Road.RoadRef
import owe.entities.passive.Roadblock.RoadblockRef
import owe.map.Cell._
import owe.map.grid.Point
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AkkaUnitSpec

import scala.concurrent.duration._

class CellSpec extends AkkaUnitSpec("CellSpec") {
  case class FixtureParam(cell: ActorRef)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam(system.actorOf(Cell.props()))))

  protected implicit val timeout: Timeout = 3.seconds

  "A Cell" should "add and remove entities" in { fixture =>
    val mapEntity = MapEntity(
      entityRef = DoodadRef(TestProbe().ref),
      parentCell = Point(0, 0),
      spec = new Doodad
    )

    fixture.cell ! AddEntity(mapEntity)
    fixture.cell ! GetEntity(mapEntity.entityRef)
    expectMsg(Some(mapEntity))

    fixture.cell ! RemoveEntity(mapEntity.entityRef)
    fixture.cell ! GetEntity(mapEntity.entityRef)
    expectMsg(None)
  }

  it should "update its modifiers" in { fixture =>
    val defaultCellData = CellData.empty
    fixture.cell ! GetCellData()
    expectMsg(defaultCellData)

    val updatedTypeData = defaultCellData.copy(
      `type` = Cell.Type.Floodplain
    )
    fixture.cell ! UpdateType(Cell.Type.Floodplain)
    fixture.cell ! GetCellData()
    expectMsg(updatedTypeData)

    val updatedDesirabilityData = updatedTypeData.copy(
      state = updatedTypeData.state.copy(desirability = Cell.Desirability.Max)
    )
    fixture.cell ! UpdateDesirability(Cell.Desirability.Max)
    fixture.cell ! GetCellData()
    expectMsg(updatedDesirabilityData)

    val updatedFertilityData = updatedDesirabilityData.copy(
      state = updatedDesirabilityData.state.copy(fertility = Fertility.Max)
    )
    fixture.cell ! UpdateFertility(Fertility.Max)
    fixture.cell ! GetCellData()
    expectMsg(updatedFertilityData)

    val updatedWaterData = updatedFertilityData.copy(
      state = updatedFertilityData.state.copy(water = Water.Max)
    )
    fixture.cell ! UpdateWater(Water.Max)
    fixture.cell ! GetCellData()
    expectMsg(updatedWaterData)

    val updateBuildAllowedData = updatedWaterData.copy(
      state = updatedWaterData.state.copy(restrictions = Cell.Restrictions.Unbuildable)
    )
    fixture.cell ! UpdateRestrictions(restrictions = Cell.Restrictions.Unbuildable)
    fixture.cell ! GetCellData()
    expectMsg(updateBuildAllowedData)
  }

  it should "return availability information" in { fixture =>
    fixture.cell ! GetCellAvailability()
    receiveOne(timeout.duration).asInstanceOf[Availability].isFree should be(true)

    val roadEntity = MapEntity(
      entityRef = RoadRef(TestProbe().ref),
      parentCell = Point(0, 0),
      spec = new Road
    )
    val roadblockEntity = roadEntity.copy(entityRef = RoadblockRef(TestProbe().ref))
    val walkerEntity = roadEntity.copy(entityRef = WalkerRef(TestProbe().ref))
    val doodadEntity = roadEntity.copy(entityRef = DoodadRef(TestProbe().ref))
    val structureEntity = roadEntity.copy(entityRef = StructureRef(TestProbe().ref))
    val resourceEntity = roadEntity.copy(entityRef = ResourceRef(TestProbe().ref))

    fixture.cell ! AddEntity(roadEntity)
    fixture.cell ! AddEntity(roadblockEntity)
    fixture.cell ! AddEntity(walkerEntity)

    fixture.cell ! GetCellAvailability()

    {
      val availability = receiveOne(timeout.duration).asInstanceOf[Availability]
      availability.isPassable should be(true)
      availability.hasRoad should be(true)
    }

    fixture.cell ! RemoveEntity(roadEntity.entityRef)
    fixture.cell ! GetCellAvailability()

    {
      val availability = receiveOne(timeout.duration).asInstanceOf[Availability]
      availability.hasRoad should be(false)
      availability.hasRoadblock should be(true)
    }

    fixture.cell ! RemoveEntity(roadblockEntity.entityRef)
    fixture.cell ! GetCellAvailability()
    receiveOne(timeout.duration).asInstanceOf[Availability].hasRoadblock should be(false)

    fixture.cell ! AddEntity(doodadEntity)
    fixture.cell ! GetCellAvailability()
    receiveOne(timeout.duration).asInstanceOf[Availability].isPassable should be(false)

    fixture.cell ! RemoveEntity(doodadEntity.entityRef)
    fixture.cell ! GetCellAvailability()
    receiveOne(timeout.duration).asInstanceOf[Availability].isPassable should be(true)

    fixture.cell ! AddEntity(structureEntity)
    fixture.cell ! GetCellAvailability()
    receiveOne(timeout.duration).asInstanceOf[Availability].isPassable should be(false)

    fixture.cell ! RemoveEntity(structureEntity.entityRef)
    fixture.cell ! GetCellAvailability()
    receiveOne(timeout.duration).asInstanceOf[Availability].isPassable should be(true)

    fixture.cell ! AddEntity(resourceEntity)
    fixture.cell ! GetCellAvailability()
    receiveOne(timeout.duration).asInstanceOf[Availability].isPassable should be(true)

    fixture.cell ! RemoveEntity(resourceEntity.entityRef)
    fixture.cell ! GetCellAvailability()
    receiveOne(timeout.duration).asInstanceOf[Availability].isPassable should be(true)
  }

  "Cell Availability" should "correctly calculate its state" in { _ =>
    val defaultCellData = CellData.empty
    val emptyData = Availability(
      cellType = defaultCellData.`type`,
      cellState = defaultCellData.state,
      entityTypes = Set.empty
    )

    emptyData.isFree should be(true)
    emptyData.isPassable should be(true)
    emptyData.hasRoad should be(false)
    emptyData.hasRoadblock should be(false)

    val dataWithRoad = emptyData.copy(entityTypes = Set(Entity.Type.Road))

    dataWithRoad.isFree should be(false)
    dataWithRoad.isPassable should be(true)
    dataWithRoad.hasRoad should be(true)
    dataWithRoad.hasRoadblock should be(false)

    val dataWithRoadblock = emptyData.copy(entityTypes = Set(Entity.Type.Roadblock))

    dataWithRoadblock.isFree should be(false)
    dataWithRoadblock.isPassable should be(true)
    dataWithRoadblock.hasRoad should be(false)
    dataWithRoadblock.hasRoadblock should be(true)

    val dataWithStructure = emptyData.copy(entityTypes = Set(Entity.Type.Structure))

    dataWithStructure.isFree should be(false)
    dataWithStructure.isPassable should be(false)
    dataWithStructure.hasRoad should be(false)
    dataWithStructure.hasRoadblock should be(false)

    val dataWithPassableEntities = emptyData.copy(
      entityTypes = Set(Entity.Type.Walker, Entity.Type.Road, Entity.Type.Roadblock)
    )

    dataWithPassableEntities.isFree should be(false)
    dataWithPassableEntities.isPassable should be(true)
    dataWithPassableEntities.hasRoad should be(true)
    dataWithPassableEntities.hasRoadblock should be(true)

    val dataInImpassibleState = emptyData.copy(
      cellState = emptyData.cellState.copy(restrictions = Cell.Restrictions.Impassable)
    )

    dataInImpassibleState.isFree should be(true)
    dataInImpassibleState.isPassable should be(false)
    dataInImpassibleState.hasRoad should be(false)
    dataInImpassibleState.hasRoadblock should be(false)
  }

  "Cell Desirability" should "support math ops" in { _ =>
    Desirability(1) + Desirability(5) should be(Desirability(6))
    Desirability(1) - Desirability(5) should be(Desirability(-4))
    Desirability(2) * 5 should be(Desirability(10))
    Desirability(2) / 2 should be(Desirability(1))
  }

  it should "support comparison ops" in { _ =>
    Desirability(1) > Desirability(5) should be(false)
    Desirability(5) > Desirability(1) should be(true)
    Desirability(1) < Desirability(5) should be(true)
    Desirability(5) < Desirability(1) should be(false)
    Desirability(1) >= Desirability(5) should be(false)
    Desirability(5) >= Desirability(5) should be(true)
    Desirability(1) <= Desirability(5) should be(true)
    Desirability(5) <= Desirability(5) should be(true)
    Desirability(1).min(Desirability(5)) should be(Desirability(1))
    Desirability(1).max(Desirability(5)) should be(Desirability(5))
  }

  "Cell Fertility" should "support comparison ops" in { _ =>
    Fertility(1).min(Fertility(5)) should be(Fertility(1))
    Fertility(1).max(Fertility(5)) should be(Fertility(5))
  }

  it should "be based on Water" in { _ =>
    Fertility(50).basedOn(Water(20)) should be(Fertility(10))
  }

  "Cell Water" should "support comparison ops" in { _ =>
    Water(1).min(Water(5)) should be(Water(1))
    Water(1).max(Water(5)) should be(Water(5))
  }
}
