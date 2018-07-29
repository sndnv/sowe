package owe.test.specs.unit.entities.active.behaviour

import org.scalatest.Outcome
import owe.entities.ActiveEntity.ActiveEntityRef
import owe.entities.ActiveEntityActor.ForwardMessage
import owe.entities.active.Walker.WalkerRef
import owe.entities.active.behaviour.UpdateExchange
import owe.map.GameMap.ForwardExchangeMessage
import owe.production.Commodity
import owe.production.Exchange.{CommodityAvailable, CommodityInTransit, CommodityRequired, UpdateCommodityState}
import owe.test.specs.unit.AkkaUnitSpec

class UpdateExchangeSpec extends AkkaUnitSpec("UpdateExchangeSpec") {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "An UpdateExchange" should "update state" in { _ =>
    implicit val parentEntity: ActiveEntityRef = WalkerRef(testActor)

    UpdateExchange.State(
      commodities = Map(
        Commodity("TestCommodity#1") -> Commodity.Amount(1),
        Commodity("TestCommodity#2") -> Commodity.Amount(2),
        Commodity("TestCommodity#3") -> Commodity.Amount(3)
      ),
      state = Commodity.State.Lost
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(Commodity("TestCommodity#1"), Commodity.Amount(1), Commodity.State.Lost)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(Commodity("TestCommodity#2"), Commodity.Amount(2), Commodity.State.Lost)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(Commodity("TestCommodity#3"), Commodity.Amount(3), Commodity.State.Lost)
        )
      )
    )

    UpdateExchange.State(
      commodities = Map(
        Commodity("TestCommodity") -> Commodity.Amount(42)
      ),
      state = Commodity.State.Produced
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(Commodity("TestCommodity"), Commodity.Amount(42), Commodity.State.Produced)
        )
      )
    )

    UpdateExchange.State(
      commodities = Map(
        Commodity("TestCommodity") -> Commodity.Amount(0)
      ),
      state = Commodity.State.Used
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          UpdateCommodityState(Commodity("TestCommodity"), Commodity.Amount(0), Commodity.State.Used)
        )
      )
    )
  }

  it should "update stats for available commodities" in { _ =>
    implicit val parentEntity: ActiveEntityRef = WalkerRef(testActor)

    UpdateExchange.Stats.availableCommodities(
      entityID = parentEntity,
      commodities = Map(
        Commodity("TestCommodity#1") -> Commodity.Amount(1),
        Commodity("TestCommodity#2") -> Commodity.Amount(2),
        Commodity("TestCommodity#3") -> Commodity.Amount(3)
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(Commodity("TestCommodity#1"), Commodity.Amount(1), parentEntity)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(Commodity("TestCommodity#2"), Commodity.Amount(2), parentEntity)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(Commodity("TestCommodity#3"), Commodity.Amount(3), parentEntity)
        )
      )
    )

    UpdateExchange.Stats.availableCommodities(
      entityID = parentEntity,
      initialCommodities = Map(
        Commodity("TestCommodity#2") -> Commodity.Amount(2),
        Commodity("TestCommodity#3") -> Commodity.Amount(3),
        Commodity("TestCommodity#4") -> Commodity.Amount(4)
      ),
      updatedCommodities = Map(
        Commodity("TestCommodity#1") -> Commodity.Amount(1),
        Commodity("TestCommodity#2") -> Commodity.Amount(20),
        Commodity("TestCommodity#3") -> Commodity.Amount(30)
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(Commodity("TestCommodity#1"), Commodity.Amount(1), parentEntity)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(Commodity("TestCommodity#2"), Commodity.Amount(20), parentEntity)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityAvailable(Commodity("TestCommodity#3"), Commodity.Amount(30), parentEntity)
        )
      )
    )
  }

  it should "update stats for required commodities" in { _ =>
    implicit val parentEntity: ActiveEntityRef = WalkerRef(testActor)

    UpdateExchange.Stats.requiredCommodities(
      entityID = parentEntity,
      consumedCommodities = Map(
        Commodity("TestCommodity#1") -> Commodity.Amount(1),
        Commodity("TestCommodity#2") -> Commodity.Amount(2),
        Commodity("TestCommodity#3") -> Commodity.Amount(3)
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityRequired(Commodity("TestCommodity#1"), Commodity.Amount(1), parentEntity)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityRequired(Commodity("TestCommodity#2"), Commodity.Amount(2), parentEntity)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityRequired(Commodity("TestCommodity#3"), Commodity.Amount(3), parentEntity)
        )
      )
    )
  }

  it should "update stats for in-transit commodities" in { _ =>
    implicit val parentEntity: ActiveEntityRef = WalkerRef(testActor)

    UpdateExchange.Stats.inTransitCommodities(
      source = parentEntity,
      destination = parentEntity,
      inTransitCommodities = Map(
        Commodity("TestCommodity#1") -> Commodity.Amount(1),
        Commodity("TestCommodity#2") -> Commodity.Amount(2),
        Commodity("TestCommodity#3") -> Commodity.Amount(3)
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityInTransit(Commodity("TestCommodity#1"), Commodity.Amount(1), parentEntity, parentEntity)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityInTransit(Commodity("TestCommodity#2"), Commodity.Amount(2), parentEntity, parentEntity)
        )
      )
    )

    expectMsg(
      ForwardMessage(
        ForwardExchangeMessage(
          CommodityInTransit(Commodity("TestCommodity#3"), Commodity.Amount(3), parentEntity, parentEntity)
        )
      )
    )
  }
}
