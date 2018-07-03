package owe.test.specs.unit.entities.active.behaviour

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import owe._
import owe.entities.ActiveEntity.MapData
import owe.entities.active.Resource.ResourceRef
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker.WalkerRef
import owe.entities.active._
import owe.map.Cell
import owe.map.grid.Point
import owe.production.{Commodity, CommodityAmount, CommodityAmountModifier}

import scala.collection.immutable.Queue

object Fixtures {
  object MockRefs {

    private implicit val mockRefsActorSystem: ActorSystem = ActorSystem("mockRefsActorSystem")

    val resource: ResourceRef = ResourceRef(TestProbe().ref)
    val structure: StructureRef = StructureRef(TestProbe().ref)
    val walker: WalkerRef = WalkerRef(TestProbe().ref)
  }

  val defaultCellState: Cell.State = Cell.State(
    desirability = CellDesirability.Neutral,
    fertility = Fertility.Max,
    water = Water.Max,
    buildingAllowed = true
  )

  val defaultMapData: MapData = MapData(
    position = Point(0, 0),
    defaultCellState
  )

  object Resource {
    import owe.entities.active.Resource.{Properties, State, StateModifiers}

    val properties: Properties = Properties(
      name = "TestResource",
      homePosition = Point(0, 0),
      commodity = Commodity("TestCommodity"),
      maxAmount = CommodityAmount(500)
    )

    val state: State = State(
      currentAmount = CommodityAmount(100),
      replenishAmount = CommodityAmount(25)
    )

    val modifiers: StateModifiers = StateModifiers(
      replenishAmount = CommodityAmountModifier(100)
    )
  }

  object Structure {
    import owe.entities.active.Structure.{Properties, State, StateModifiers, _}

    object Producing {
      val properties: Properties = Properties(
        homePosition = Point(0, 0),
        name = "TestProducingStructure",
        walkers = NoWalkers,
        stages = SingleStage(
          stage = StageProperties(
            maxLife = Life(100),
            maxPeople = 15,
            minDesirability = CellDesirability.Neutral,
            commodityShortageLimit = 0
          )
        )
      )

      val state: State = State(
        risk = RiskState(fire = RiskAmount(0), damage = RiskAmount(0)),
        commodities = CommoditiesState(
          available = Map.empty,
          limits = Map(Commodity("TestCommodity") -> CommodityAmount(100))
        ),
        housing = NoHousing,
        production = ProductionState(
          employees = 15,
          labour = LabourState.Found,
          rates = Map(Commodity("TestCommodity") -> CommodityAmount(25))
        ),
        currentStage = DefaultStage,
        currentLife = Life(100),
        walkers = NoWalkers
      )

      val modifiers: StateModifiers = StateModifiers(
        risk = RiskModifier(fire = RiskAmount(3), damage = RiskAmount(5)),
        commodities = CommoditiesModifier(usageRates = Map.empty),
        production = ProductionModifier(rates = Map(Commodity("TestCommodity") -> CommodityAmountModifier(100))),
        housing = NoHousing
      )
    }

    object Housing {
      val properties: Properties = Properties(
        homePosition = Point(0, 0),
        name = "TestHousingStructure",
        walkers = WalkersProperties(
          generators = Map.empty
        ),
        stages = MultiStage(
          stages = Seq(
            StageProperties(
              maxLife = Life(100),
              maxPeople = 5,
              minDesirability = CellDesirability.Neutral,
              commodityShortageLimit = 10
            ),
            StageProperties(
              maxLife = Life(150),
              maxPeople = 15,
              minDesirability = CellDesirability(4),
              commodityShortageLimit = 5
            ),
            StageProperties(
              maxLife = Life(200),
              maxPeople = 50,
              minDesirability = CellDesirability.Max,
              commodityShortageLimit = 3
            )
          )
        )
      )

      val state: State = State(
        risk = RiskState(fire = RiskAmount(0), damage = RiskAmount(0)),
        commodities = CommoditiesState(
          available = Map.empty,
          limits = Map(
            Commodity("TestCommodity#1") -> CommodityAmount(100),
            Commodity("TestCommodity#2") -> CommodityAmount(10),
            Commodity("TestCommodity#3") -> CommodityAmount(200)
          )
        ),
        housing = HousingState(
          occupants = 0,
          commodityShortage = 0,
          education = Map(
            EducationEntry("Entry#1") -> EducationLevel(current = 0, minimal = 0, required = 100)
          ),
          entertainment = Map(
            EntertainmentEntry("Entry#2") -> EntertainmentLevel(current = 0, minimal = 0, required = 100)
          ),
          religion = Map(
            ReligionEntry("Entry#3") -> ReligionLevel(current = 0, minimal = 0, required = 100)
          ),
          healthcare = Map(
            HealthcareEntry("Entry#4") -> HealthcareLevel(current = 0, minimal = 0, required = 100)
          ),
          civilService = Map(
            CivilServiceEntry("Entry#5") -> CivilServiceLevel(current = 0, minimal = 0, required = 100)
          )
        ),
        production = NoProduction,
        currentStage = CurrentStage(0),
        currentLife = Life(100),
        walkers = WalkersState(
          state = Map.empty
        )
      )

      val modifiers: StateModifiers = StateModifiers(
        risk = RiskModifier(fire = RiskAmount(3), damage = RiskAmount(5)),
        commodities = CommoditiesModifier(
          usageRates = Map(
            Commodity("TestCommodity#1") -> CommodityAmount(5),
            Commodity("TestCommodity#2") -> CommodityAmount(1),
            Commodity("TestCommodity#3") -> CommodityAmount(5)
          )
        ),
        production = NoProduction,
        housing = HousingModifier(
          education = Map(EducationEntry("Entry#1") -> EducationLevelModifier(10)),
          entertainment = Map(EntertainmentEntry("Entry#2") -> EntertainmentLevelModifier(20)),
          religion = Map(ReligionEntry("Entry#3") -> ReligionLevelModifier(30)),
          healthcare = Map(HealthcareEntry("Entry#4") -> HealthcareLevelModifier(40)),
          civilService = Map(CivilServiceEntry("Entry#5") -> CivilServiceLevelModifier(50))
        )
      )
    }
  }

  object Walker {
    import owe.entities.active.Walker._

    val properties: Properties = Properties(
      parent = None,
      homePosition = Point(0, 0),
      name = "TestWalker",
      maxLife = Life(500),
      movementSpeed = Speed(150),
      maxRoamingDistance = Distance(50),
      attack = AttackProperties(
        rate = AttackRate(3),
        damage = AttackDamage(50),
        distance = Distance(25),
        target = _ => true
      )
    )

    val state: State = State(
      currentLife = Life(100),
      distanceCovered = Distance(0),
      destinationPath = Queue.empty,
      commodities = CommoditiesState(
        available = Map.empty,
        limits = Map(Commodity("TestCommodity") -> CommodityAmount(100))
      ),
      path = Queue.empty,
      mode = MovementMode.Advancing
    )

    val modifiers: StateModifiers = StateModifiers(
      movementSpeed = SpeedModifier(100),
      maxRoamingDistance = DistanceModifier(100),
      attack = AttackModifiers(
        rate = AttackRateModifier(200),
        damage = AttackDamageModifier(50),
        distance = DistanceModifier(100)
      )
    )
  }
}
