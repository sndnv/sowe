package owe.test.specs.unit.entities.passive

import org.scalatest.Outcome
import owe.entities.Entity
import owe.entities.Entity.Desirability
import owe.entities.passive.Roadblock
import owe.map.Cell
import owe.test.specs.unit.UnitSpec

class RoadblockSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A Roadblock" should "have correct basic entity properties" in { _ =>
    val entity = new Roadblock
    entity.`size` should be(Entity.Size(height = 1, width = 1))
    entity.`type` should be(Entity.Type.Roadblock)
    entity.`desirability` should be(Desirability.Neutral)
  }

  it should "have cell availability requirements" in { _ =>
    val entity = new Roadblock
    val availability = Cell.Availability(
      cellType = Cell.Type.Land,
      cellState = Cell.CellData.empty.state,
      entityTypes = Set(Entity.Type.Road)
    )

    entity.acceptsAvailability(availability) should be(true)

    entity.acceptsAvailability(
      availability.copy(entityTypes = Set.empty)
    ) should be(false)

    entity.acceptsAvailability(
      availability.copy(entityTypes = Set(Entity.Type.Road, Entity.Type.Roadblock))
    ) should be(false)

    entity.acceptsAvailability(
      availability.copy(entityTypes = Set(Entity.Type.Road, Entity.Type.Walker))
    ) should be(false)
  }
}
