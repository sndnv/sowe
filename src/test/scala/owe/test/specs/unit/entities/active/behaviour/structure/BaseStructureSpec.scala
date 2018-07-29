package owe.test.specs.unit.entities.active.behaviour.structure

import akka.actor.Props
import org.scalatest.Outcome
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.StructureData
import owe.entities.ActiveEntityActor._
import owe.entities.Entity.ProcessCommodities
import owe.entities.active.Structure
import owe.entities.active.Structure.CommoditiesState
import owe.entities.active.attributes.Life
import owe.entities.active.behaviour.structure.producing.ProducingStructure
import owe.map.GameMap.{DestroyEntity, ForwardExchangeMessage}
import owe.production.Commodity
import owe.production.Exchange.UpdateCommodityState
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, StructureParentEntity}

class BaseStructureSpec extends AkkaUnitSpec("BaseStructureSpec") {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A BaseStructure" should "destroy itself when it has insufficient life" in { _ =>
    val parentEntity = system.actorOf(
      StructureParentEntity.props(
        testActor,
        Props(
          new ProducingStructure {}
        )
      )
    )

    val structureData = StructureData(
      properties = Fixtures.Structure.Producing.properties,
      state = Fixtures.Structure.Producing.state.copy(
        risk = Structure.NoRisk,
        currentLife = Life(0),
        commodities = CommoditiesState(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(200)),
          limits = Map.empty
        )
      ),
      modifiers = Fixtures.Structure.Producing.modifiers.copy(
        risk = Structure.NoRisk
      ),
      Fixtures.MockRefs.structure
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = structureData
    )

    // no state update expected when destroying
    expectMsg(
      BehaviourTickProcessed(
        structureData.state
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(
            Commodity("TestCommodity"),
            Commodity.Amount(200),
            Commodity.State.Lost
          )
        )
      )
    )

    expectMsg(
      ForwardMessage(DestroyEntity(structureData.id))
    )

    // should ignore instructions
    parentEntity ! ApplyInstructions(structureData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should ignore messages
    parentEntity ! ApplyMessages(
      entity = structureData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        structureData.state
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = structureData
    )

    // no state update expected when destroying
    expectMsg(
      BehaviourTickProcessed(
        structureData.state
      )
    )
  }
}
