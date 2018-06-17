package owe.test.specs.unit.entities.active.behaviour.walker.transformations

import org.scalatest.FutureOutcome
import owe.entities.ActiveEntity.WalkerData
import owe.entities.Entity.{ProcessAttack, ProcessCommodities}
import owe.entities.active.Walker.CommoditiesState
import owe.entities.active.behaviour.walker.transformations.ProcessedUpdateMessages
import owe.entities.active.{AttackDamage, Life}
import owe.production.{Commodity, CommodityAmount}
import owe.test.specs.unit.AsyncUnitSpec
import owe.test.specs.unit.entities.active.behaviour.Fixtures

class ProcessedUpdateMessagesSpec extends AsyncUnitSpec {
  case class FixtureParam(transformer: ProcessedUpdateMessages, walker: WalkerData)

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val transformer = new ProcessedUpdateMessages {}

    val walker = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    withFixture(test.toNoArgAsyncTest(FixtureParam(transformer, walker)))
  }

  "A ProcessedUpdateMessages transformation" should "handle incoming and outgoing commodities" in { fixture =>
    val commoditiesState = fixture.walker.state.commodities.asInstanceOf[CommoditiesState]

    val walkerWitMoreAvailableCommodities = fixture.walker.copy(
      state = fixture.walker.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> CommodityAmount(60))
        )
      )
    )

    val walkerWithLessAvailableCommodities = fixture.walker.copy(
      state = fixture.walker.state.copy(
        commodities = commoditiesState.copy(
          available = Map(Commodity("TestCommodity") -> CommodityAmount(10))
        )
      )
    )

    for {
      noUpdateState <- fixture.transformer.withProcessedUpdateMessages(
        fixture.walker,
        pendingMessages = Seq.empty
      )
      addToWalkerWithNoCommodities <- fixture.transformer.withProcessedUpdateMessages(
        fixture.walker,
        pendingMessages = Seq(
          ProcessCommodities(Seq((Commodity("TestCommodity"), CommodityAmount(42))))
        )
      )
      addToWalkerWithMoreCommodities <- fixture.transformer.withProcessedUpdateMessages(
        walkerWitMoreAvailableCommodities,
        pendingMessages = Seq(
          ProcessCommodities(Seq((Commodity("TestCommodity"), CommodityAmount(42))))
        )
      )
      removeFromWalkerWithMoreCommodities <- fixture.transformer.withProcessedUpdateMessages(
        walkerWitMoreAvailableCommodities,
        pendingMessages = Seq(
          ProcessCommodities(Seq((Commodity("TestCommodity"), CommodityAmount(-42))))
        )
      )
      removeFromWalkerWithLessCommodities <- fixture.transformer.withProcessedUpdateMessages(
        walkerWithLessAvailableCommodities,
        pendingMessages = Seq(
          ProcessCommodities(Seq((Commodity("TestCommodity"), CommodityAmount(-42))))
        )
      )
      removeFromWalkerWithNoCommodities <- fixture.transformer.withProcessedUpdateMessages(
        fixture.walker,
        pendingMessages = Seq(
          ProcessCommodities(Seq((Commodity("TestCommodity"), CommodityAmount(-42))))
        )
      )
    } yield {
      noUpdateState should be(fixture.walker.state)

      addToWalkerWithNoCommodities should be(
        fixture.walker.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(42))
          )
        )
      )

      addToWalkerWithMoreCommodities should be(
        walkerWitMoreAvailableCommodities.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(100))
          )
        )
      )

      removeFromWalkerWithMoreCommodities should be(
        walkerWitMoreAvailableCommodities.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(18))
          )
        )
      )

      removeFromWalkerWithLessCommodities should be(
        walkerWithLessAvailableCommodities.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(0))
          )
        )
      )

      removeFromWalkerWithNoCommodities should be(
        fixture.walker.state.copy(
          commodities = commoditiesState.copy(
            available = Map(Commodity("TestCommodity") -> CommodityAmount(0))
          )
        )
      )
    }
  }

  it should "handle attacks" in { fixture =>
    for {
      noDamageState <- fixture.transformer.withProcessedUpdateMessages(
        fixture.walker,
        pendingMessages = Seq(
          ProcessAttack(AttackDamage(0))
        )
      )
      minorDamageState <- fixture.transformer.withProcessedUpdateMessages(
        fixture.walker,
        pendingMessages = Seq(
          ProcessAttack(AttackDamage(5))
        )
      )
      completeDamageState <- fixture.transformer.withProcessedUpdateMessages(
        fixture.walker,
        pendingMessages = Seq(
          ProcessAttack(AttackDamage(100))
        )
      )
      extraDamageState <- fixture.transformer.withProcessedUpdateMessages(
        fixture.walker,
        pendingMessages = Seq(
          ProcessAttack(AttackDamage(150))
        )
      )
    } yield {
      noDamageState should be(fixture.walker.state)
      minorDamageState should be(fixture.walker.state.copy(currentLife = Life(95)))
      completeDamageState should be(fixture.walker.state.copy(currentLife = Life(0)))
      extraDamageState should be(fixture.walker.state.copy(currentLife = Life(0)))
    }
  }
}
