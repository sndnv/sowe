package owe.test.specs.unit.entities.active.behaviour.structure.transformations

import org.scalatest.Outcome
import owe.entities.ActiveEntity.StructureData
import owe.entities.Entity._
import owe.entities.active.Structure.{CommoditiesState, HousingState, LabourState, ProductionState}
import owe.entities.active.behaviour.structure.transformations.ProcessedUpdateMessages
import owe.entities.active.attributes.{AttackDamage, Life}
import owe.production.Commodity
import owe.test.specs.unit.UnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ProcessedUpdateMessagesSpec extends UnitSpec {
  case class FixtureParam(
    transformer: ProcessedUpdateMessages
  )

  def withFixture(test: OneArgTest): Outcome = {
    val transformer = new ProcessedUpdateMessages {}

    withFixture(test.toNoArgTest(FixtureParam(transformer)))
  }

  "A ProcessedUpdateMessages transformation" should "handle incoming and outgoing commodities" in { fixture =>
    val structure = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state,
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    val commoditiesState = structure.state.commodities.asInstanceOf[CommoditiesState]

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq.empty
    ) should be(structure.state)

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(42))))
      )
    ) should be(
      structure.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(42))
        )
      )
    )

    {
      val structureWithAvailableCommodities = structure.copy(
        state = structure.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(60))
          )
        )
      )

      fixture.transformer.withProcessedUpdateMessages(
        structureWithAvailableCommodities,
        pendingMessages = Seq(
          ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(42))))
        )
      ) should be(
        structureWithAvailableCommodities.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    }

    {
      val structureWithAvailableCommodities = structure.copy(
        state = structure.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(60))
          )
        )
      )

      fixture.transformer.withProcessedUpdateMessages(
        structureWithAvailableCommodities,
        pendingMessages = Seq(
          ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(-42))))
        )
      ) should be(
        structureWithAvailableCommodities.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(18))
          )
        )
      )
    }

    {
      val structureWithAvailableCommodities = structure.copy(
        state = structure.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10))
          )
        )
      )

      fixture.transformer.withProcessedUpdateMessages(
        structureWithAvailableCommodities,
        pendingMessages = Seq(
          ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(-42))))
        )
      ) should be(
        structureWithAvailableCommodities.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(0))
          )
        )
      )
    }

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(-42))))
      )
    ) should be(
      structure.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(0))
        )
      )
    )
  }

  it should "occupancy updates" in { fixture =>
    val structure = StructureData(
      Fixtures.Structure.Housing.properties,
      Fixtures.Structure.Housing.state,
      Fixtures.Structure.Housing.modifiers,
      Fixtures.MockRefs.structure
    )

    val housingState = structure.state.housing.asInstanceOf[HousingState]

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessOccupantsUpdate(0)
      )
    ) should be(
      structure.state
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessOccupantsUpdate(1)
      )
    ) should be(
      structure.state.copy(
        housing = housingState.copy(
          occupants = 1
        )
      )
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessOccupantsUpdate(10)
      )
    ) should be(
      structure.state.copy(
        housing = housingState.copy(
          occupants = 5
        )
      )
    )
  }

  it should "handle labour updates" in { fixture =>
    val productionState = Fixtures.Structure.Producing.state.production.asInstanceOf[ProductionState]

    val structure = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state.copy(
        production = productionState.copy(employees = 0)
      ),
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessLabourUpdate(0)
      )
    ) should be(
      structure.state
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessLabourUpdate(1)
      )
    ) should be(
      structure.state.copy(
        production = productionState.copy(
          employees = 1
        )
      )
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessLabourUpdate(20)
      )
    ) should be(
      structure.state.copy(
        production = productionState.copy(
          employees = 15
        )
      )
    )
  }

  it should "handle attacks" in { fixture =>
    val structure = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state,
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessAttack(AttackDamage(0))
      )
    ) should be(
      structure.state
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessAttack(AttackDamage(5))
      )
    ) should be(
      structure.state.copy(
        currentLife = Life(95)
      )
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessAttack(AttackDamage(100))
      )
    ) should be(
      structure.state.copy(
        currentLife = Life(0)
      )
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessAttack(AttackDamage(150))
      )
    ) should be(
      structure.state.copy(
        currentLife = Life(0)
      )
    )
  }

  it should "handle labour discovery" in { fixture =>
    val structure = StructureData(
      Fixtures.Structure.Producing.properties,
      Fixtures.Structure.Producing.state,
      Fixtures.Structure.Producing.modifiers,
      Fixtures.MockRefs.structure
    )

    val productionState = structure.state.production.asInstanceOf[ProductionState]

    fixture.transformer.withProcessedUpdateMessages(
      structure,
      pendingMessages = Seq(
        ProcessLabourFound()
      )
    ) should be(
      structure.state
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure.copy(
        state = structure.state.copy(
          production = productionState.copy(
            labour = LabourState.None
          )
        )
      ),
      pendingMessages = Seq(
        ProcessLabourFound()
      )
    ) should be(
      structure.state.copy(
        production = productionState.copy(
          labour = LabourState.Found
        )
      )
    )

    fixture.transformer.withProcessedUpdateMessages(
      structure.copy(
        state = structure.state.copy(
          production = productionState.copy(
            labour = LabourState.Looking
          )
        )
      ),
      pendingMessages = Seq(
        ProcessLabourFound()
      )
    ) should be(
      structure.state.copy(
        production = productionState.copy(
          labour = LabourState.Found
        )
      )
    )
  }
}
