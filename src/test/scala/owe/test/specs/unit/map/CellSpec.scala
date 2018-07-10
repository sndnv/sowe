package owe.test.specs.unit.map

import akka.actor.ActorRef
import akka.testkit.TestProbe
import org.scalatest.Outcome
import owe.entities.Entity
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.passive.Doodad.DoodadRef
import owe.entities.passive.Road.RoadRef
import owe.entities.passive.Roadblock.RoadblockRef
import owe.map.Cell.{UpdateBuildAllowed, _}
import owe.map.grid.Point
import owe.map.{Cell, MapEntity}
import owe.test.specs.unit.AkkaUnitSpec

class CellSpec extends AkkaUnitSpec("CellSpec") {
  case class FixtureParam(cell: ActorRef)

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam(system.actorOf(Cell.props()))))

  "A Cell" should "add and remove entities" in { fixture =>
    val mapEntity = MapEntity(
      entityRef = DoodadRef(TestProbe().ref),
      parentCell = Point(0, 0),
      size = Entity.Size(1, 1),
      desirability = Entity.Desirability.Min
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

    val updatedDesirabilityData = defaultCellData.copy(
      state = defaultCellData.state.copy(desirability = Cell.Desirability.Max)
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
      state = updatedWaterData.state.copy(buildingAllowed = false)
    )
    fixture.cell ! UpdateBuildAllowed(buildingAllowed = false)
    fixture.cell ! GetCellData()
    expectMsg(updateBuildAllowedData)
  }

  it should "return availability information" in { fixture =>
    fixture.cell ! GetCellAvailability()
    expectMsg(Cell.Availability.Buildable)

    val roadEntity = MapEntity(
      entityRef = RoadRef(TestProbe().ref),
      parentCell = Point(0, 0),
      size = Entity.Size(1, 1),
      desirability = Entity.Desirability.Min
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
    expectMsg(Cell.Availability.Passable)

    fixture.cell ! HasRoad()
    expectMsg(true)

    fixture.cell ! RemoveEntity(roadEntity.entityRef)
    fixture.cell ! HasRoad()
    expectMsg(false)

    fixture.cell ! AddEntity(doodadEntity)
    fixture.cell ! GetCellAvailability()
    expectMsg(Cell.Availability.Occupied)
    fixture.cell ! RemoveEntity(doodadEntity.entityRef)
    fixture.cell ! GetCellAvailability()
    expectMsg(Cell.Availability.Passable)

    fixture.cell ! AddEntity(structureEntity)
    fixture.cell ! GetCellAvailability()
    expectMsg(Cell.Availability.Occupied)
    fixture.cell ! RemoveEntity(structureEntity.entityRef)
    fixture.cell ! GetCellAvailability()
    expectMsg(Cell.Availability.Passable)

    fixture.cell ! AddEntity(resourceEntity)
    fixture.cell ! GetCellAvailability()
    expectMsg(Cell.Availability.Occupied)
    fixture.cell ! RemoveEntity(resourceEntity.entityRef)
    fixture.cell ! GetCellAvailability()
    expectMsg(Cell.Availability.Passable)

    Cell.requiredAvailability(Entity.Type.Doodad) should be(Availability.Buildable)
    Cell.requiredAvailability(Entity.Type.Road) should be(Availability.Buildable)
    Cell.requiredAvailability(Entity.Type.Roadblock) should be(Availability.Passable)
    Cell.requiredAvailability(Entity.Type.Resource) should be(Availability.Buildable)
    Cell.requiredAvailability(Entity.Type.Structure) should be(Availability.Buildable)
    Cell.requiredAvailability(Entity.Type.Walker) should be(Availability.Passable)
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
