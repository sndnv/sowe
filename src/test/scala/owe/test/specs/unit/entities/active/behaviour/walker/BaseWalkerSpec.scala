package owe.test.specs.unit.entities.active.behaviour.walker

import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor.Props
import akka.testkit.TestProbe
import org.scalatest.Outcome
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.WalkerData
import owe.entities.ActiveEntityActor._
import owe.entities.Entity.ProcessCommodities
import owe.entities.active.Structure.StructureRef
import owe.entities.active.Walker._
import owe.entities.active.attributes._
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.map.GameMap._
import owe.map.grid.Point
import owe.production.Commodity
import owe.production.Exchange.UpdateCommodityState
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, WalkerParentEntity}

class BaseWalkerSpec extends AkkaUnitSpec("BaseWalkerSpec") {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A BaseWalker " should "have attacking behaviour" in { _ =>
    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testActor,
        Props(
          new BaseWalker {
            override protected def behaviour: Behaviour = attacking(() => idling())
          }
        )
      )
    )

    val walkerData = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    // should process instructions
    parentEntity ! ApplyInstructions(walkerData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should process messages
    parentEntity ! ApplyMessages(
      entity = walkerData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Walker.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    )

    // is in attacking state; should process attack
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state,
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      )
    )

    val damage = receiveOne(max = 3.seconds) match {
      case ForwardMessage(attack: AttackEntity) => attack.damage
      case message                              => fail(s"Unexpected message received: [$message]")
    }

    damage should be(AttackDamage(25))

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Walker.state
      )
    )

    // is in attacking state; without target, should not attack and should switch to idling
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties.copy(
          attack = AttackProperties(
            rate = AttackRate(3),
            damage = AttackDamage(50),
            distance = Distance(25),
            target = _ => false
          )
        ),
        Fixtures.Walker.state,
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Walker.state
      )
    )

    // is in idling state; should not attack
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties.copy(attack = NoAttack),
        Fixtures.Walker.state,
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        )
      )
    )
  }

  it should "have idling behaviour" in { _ =>
    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testActor,
        Props(
          new BaseWalker {
            override protected def behaviour: Behaviour = idling()
          }
        )
      )
    )

    val walkerData = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    // should process instructions
    parentEntity ! ApplyInstructions(walkerData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should process messages
    parentEntity ! ApplyMessages(
      entity = walkerData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Walker.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    )

    // is in idling state; cannot attack and should remain idling
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties.copy(attack = NoAttack),
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        )
      )
    )

    // is in idling state; can attack and should switch to attacking state
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        )
      )
    )

    // is in attacking state and should process attack
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      )
    )

    val damage = receiveOne(max = 3.seconds) match {
      case ForwardMessage(attack: AttackEntity) => attack.damage
      case message                              => fail(s"Unexpected message received: [$message]")
    }

    damage should be(AttackDamage(25))

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        )
      )
    )
  }

  it should "have roaming behaviour" in { _ =>
    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testActor,
        Props(
          new BaseWalker {
            override protected def behaviour: Behaviour = roaming(NoAction)
          }
        )
      )
    )

    val walkerData = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    // should process instructions
    parentEntity ! ApplyInstructions(walkerData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should process messages
    parentEntity ! ApplyMessages(
      entity = walkerData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Walker.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    )

    val roamingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(0, 0),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack,
        traversalMode = TraversalMode.OnLand
      ),
      state = State(
        currentPosition = Point(0, 0),
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue(Point(0, 1), Point(0, 2)),
        mode = MovementMode.Roaming
      ),
      modifiers = StateModifiers(
        movementSpeed = Speed.Modifier(100),
        maxRoamingDistance = Distance.Modifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    // is in roaming state; cannot attack, should remain roaming and move to next position
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData
    )

    expectMsg(
      ForwardMessage(
        MoveEntity(
          roamingWalkerData.id,
          Point(0, 1)
        )
      )
    )

    val updatedStateWithMovement = roamingWalkerData.state.copy(
      distanceCovered = Distance(1),
      path = Queue(Point(0, 2))
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateWithMovement
      )
    )

    // is in roaming state; cannot attack, should remain roaming and move to next position
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData.copy(state = updatedStateWithMovement)
    )

    expectMsg(
      ForwardMessage(
        MoveEntity(
          roamingWalkerData.id,
          Point(0, 2)
        )
      )
    )

    val updatedStateWithPathComplete = roamingWalkerData.state.copy(
      distanceCovered = Distance(2),
      path = Queue.empty
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateWithPathComplete
      )
    )

    // is in roaming state; cannot attack; current roaming path complete; generate new roaming path
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData.copy(state = updatedStateWithPathComplete)
    )

    val updatedStateWithNewRoamingPath = roamingWalkerData.state.copy(
      distanceCovered = Distance(2),
      path = Queue(Point(0, 1), Point(0, 2), Point(1, 2))
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateWithNewRoamingPath
      )
    )

    // is in roaming state; cannot attack; current roaming path complete; generate new roaming path
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData.copy(state = updatedStateWithNewRoamingPath)
    )

    expectMsg(
      ForwardMessage(
        MoveEntity(
          roamingWalkerData.id,
          Point(0, 1)
        )
      )
    )

    val updatedStateWithUsingNewRoamingPath = roamingWalkerData.state.copy(
      distanceCovered = Distance(3),
      path = Queue(Point(0, 2), Point(1, 2))
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateWithUsingNewRoamingPath
      )
    )

    // is in roaming state; should not move to next position (no free path)
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData.copy(
        properties = roamingWalkerData.properties.copy(
          maxRoamingDistance = Distance(2)
        ),
        state = roamingWalkerData.state.copy(
          distanceCovered = Distance(1),
          path = Queue.empty
        )
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        roamingWalkerData.state.copy(
          distanceCovered = Distance(1),
          path = Queue.empty,
          mode = MovementMode.Idling
        )
      )
    )

    // is in roaming state; can attack and should switch to attacking state
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state.copy(
          mode = MovementMode.Roaming
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Walker.state.copy(mode = MovementMode.Roaming)
      )
    )

    // is in attacking state and should process attack
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      )
    )

    val damage = receiveOne(max = 3.seconds) match {
      case ForwardMessage(attack: AttackEntity) => attack.damage
      case message                              => fail(s"Unexpected message received: [$message]")
    }

    damage should be(AttackDamage(25))

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Walker.state.copy(mode = MovementMode.Idling)
      )
    )

    // is in roaming state; should not move to next position (max distance reached)
    val returningEntityData = roamingWalkerData.copy(
      properties = roamingWalkerData.properties.copy(
        maxRoamingDistance = Distance(2)
      ),
      state = roamingWalkerData.state.copy(
        distanceCovered = Distance(3),
        path = Queue.empty
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = returningEntityData
    )

    expectMsg(
      BehaviourTickProcessed(
        returningEntityData.state
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = returningEntityData
    )

    expectMsg(
      BehaviourTickProcessed(
        returningEntityData.state
      )
    )

  }

  it should "have advancing behaviour (go to point then go home)" in { _ =>
    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testActor,
        Props(
          new BaseWalker {
            override protected def behaviour: Behaviour = advancing(
              mode = MovementMode.Advancing,
              destination = DestinationPoint(Point(2, 2)),
              destinationActions = Seq(GoHome())
            )
          }
        )
      )
    )

    val walkerData = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    // should process instructions
    parentEntity ! ApplyInstructions(walkerData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should process messages
    parentEntity ! ApplyMessages(
      entity = walkerData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Walker.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    )

    val advancingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(1, 1),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack,
        traversalMode = TraversalMode.OnLand
      ),
      state = State(
        currentPosition = Point(1, 1),
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue(Point(2, 2)),
        mode = MovementMode.Advancing
      ),
      modifiers = StateModifiers(
        movementSpeed = Speed.Modifier(100),
        maxRoamingDistance = Distance.Modifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    // is in advancing state; is not at destination, should move
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 2)),
      entity = advancingWalkerData
    )

    expectMsg(
      ForwardMessage(
        MoveEntity(
          advancingWalkerData.id,
          Point(2, 2)
        )
      )
    )

    val updatedStateWithMovement = advancingWalkerData.state.copy(
      distanceCovered = Distance(1),
      path = Queue.empty
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateWithMovement
      )
    )

    // is in advancing state; is at destination, should switch to acting
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(2, 2)),
      entity = advancingWalkerData.copy(state = updatedStateWithMovement)
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateWithMovement
      )
    )

    // is in acting state; is at destination, should switch to advancing (going home)
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(2, 2)),
      entity = advancingWalkerData.copy(state = updatedStateWithMovement)
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateWithMovement
      )
    )

    // is in advancing state; is at destination, should be going home
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(2, 2)),
      entity = advancingWalkerData.copy(state = updatedStateWithMovement)
    )

    val updatedStateGoingHome = updatedStateWithMovement.copy(
      distanceCovered = Distance(2),
      path = Queue(Point(2, 0), Point(2, 1), Point(1, 1)),
      mode = MovementMode.Returning
    )

    expectMsg(
      ForwardMessage(
        MoveEntity(
          advancingWalkerData.id,
          Point(1, 0)
        )
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateGoingHome
      )
    )
  }

  it should "have advancing behaviour (go to entity)" in { _ =>
    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testActor,
        Props(
          new BaseWalker {
            override protected def behaviour: Behaviour = advancing(
              mode = MovementMode.Advancing,
              destination = DestinationEntity(WalkerRef(TestProbe().ref)),
              destinationActions = Seq(GoHome())
            )
          }
        )
      )
    )

    val walkerData = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    // should process instructions
    parentEntity ! ApplyInstructions(walkerData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should process messages
    parentEntity ! ApplyMessages(
      entity = walkerData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Walker.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    )

    val advancingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(1, 1),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack,
        traversalMode = TraversalMode.OnLand
      ),
      state = State(
        currentPosition = Point(1, 1),
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue.empty,
        mode = MovementMode.Advancing
      ),
      modifiers = StateModifiers(
        movementSpeed = Speed.Modifier(100),
        maxRoamingDistance = Distance.Modifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    // is in advancing state; is at home and should get path to destination entity
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = advancingWalkerData
    )

    val updatedStateGoingToEntity = advancingWalkerData.state.copy(
      distanceCovered = Distance(1),
      path = Queue(Point(2, 0), Point(2, 1), Point(1, 1), Point(0, 1)),
      mode = MovementMode.Advancing
    )

    expectMsg(
      ForwardMessage(
        MoveEntity(
          advancingWalkerData.id,
          Point(1, 0)
        )
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateGoingToEntity
      )
    )

    // is in advancing state; cannot proceed on path; should generate new and proceed
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = advancingWalkerData.copy(state = advancingWalkerData.state.copy(path = Queue(Point(0, 0))))
    )

    expectMsg(
      ForwardMessage(
        MoveEntity(
          advancingWalkerData.id,
          Point(1, 0)
        )
      )
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateGoingToEntity
      )
    )
  }

  it should "have advancing behaviour (fail to go to unreachable destination)" in { _ =>
    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testActor,
        Props(
          new BaseWalker {
            override protected def behaviour: Behaviour = advancing(
              mode = MovementMode.Advancing,
              destination = DestinationEntity(StructureRef(TestProbe().ref)),
              destinationActions = Seq(GoHome())
            )
          }
        )
      )
    )

    val walkerData = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    // should process instructions
    parentEntity ! ApplyInstructions(walkerData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should process messages
    parentEntity ! ApplyMessages(
      entity = walkerData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Walker.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    )

    val advancingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(1, 1),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack,
        traversalMode = TraversalMode.OnLand
      ),
      state = State(
        currentPosition = Point(1, 1),
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue.empty,
        mode = MovementMode.Advancing
      ),
      modifiers = StateModifiers(
        movementSpeed = Speed.Modifier(100),
        maxRoamingDistance = Distance.Modifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    // is in advancing state; is at home and should fail get path to unreachable entity
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = advancingWalkerData
    )

    val updatedStateWithNoPath = advancingWalkerData.state.copy(
      path = Queue.empty,
      mode = MovementMode.Idling
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateWithNoPath
      )
    )
  }

  it should "have acting behaviour" in { _ =>
    val actions = Seq(
      DoOperation(walker => Future.successful(walker.state.copy(currentLife = Life(42)))),
      DoRepeatableOperation(
        op = walker => Future.successful(walker.state.copy(currentLife = walker.state.currentLife + Life(5000))),
        repeat = walker => walker.state.currentLife < Life(9000)
      ),
      GoToPoint(Point(1, 1)),
      GoToEntity(Fixtures.MockRefs.structure),
      GoHome(),
      NoAction
    )

    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testActor,
        Props(
          new BaseWalker {
            override protected def behaviour: Behaviour = acting(actions)
          }
        )
      )
    )

    val walkerData = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    // should process instructions
    parentEntity ! ApplyInstructions(walkerData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should process messages
    parentEntity ! ApplyMessages(
      entity = walkerData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Walker.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    )

    val actingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(1, 1),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack,
        traversalMode = TraversalMode.OnLand
      ),
      state = State(
        currentPosition = Point(1, 1),
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue.empty,
        mode = MovementMode.Idling
      ),
      modifiers = StateModifiers(
        movementSpeed = Speed.Modifier(100),
        maxRoamingDistance = Distance.Modifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData
    )

    val updatedStateAfterDoOperation = actingWalkerData.state.copy(
      currentLife = Life(42)
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateAfterDoOperation
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData.copy(state = updatedStateAfterDoOperation)
    )

    val updatedStateAfterOneRepeatableOperation = actingWalkerData.state.copy(
      currentLife = Life(5042)
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateAfterOneRepeatableOperation
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData.copy(state = updatedStateAfterOneRepeatableOperation)
    )

    val updatedStateAfterTwoRepeatableOperations = actingWalkerData.state.copy(
      currentLife = Life(10042)
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateAfterTwoRepeatableOperations
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData.copy(state = updatedStateAfterTwoRepeatableOperations)
    )

    // repeatable operation not applicable; no state update is expected
    expectMsg(
      BehaviourTickProcessed(
        updatedStateAfterTwoRepeatableOperations
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData.copy(state = updatedStateAfterTwoRepeatableOperations)
    )

    // just transitioned to advancing; no state update is expected
    expectMsg(
      BehaviourTickProcessed(
        updatedStateAfterTwoRepeatableOperations
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(
        state = updatedStateAfterTwoRepeatableOperations.copy(path = Queue.empty)
      )
    )

    val updatedStateAfterGoingToPoint = updatedStateAfterTwoRepeatableOperations.copy(
      path = Queue.empty
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateAfterGoingToPoint
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = updatedStateAfterGoingToPoint.copy(path = Queue.empty))
    )

    val updatedStateAfterGoingToEntity = updatedStateAfterGoingToPoint.copy(
      path = Queue.empty
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateAfterGoingToEntity
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(0, 0)),
      entity = actingWalkerData.copy(state = updatedStateAfterGoingToEntity)
    )

    val updatedStateAfterGoingHome = updatedStateAfterGoingToEntity.copy(
      path = Queue.empty
    )

    expectMsg(
      BehaviourTickProcessed(
        updatedStateAfterGoingHome
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = updatedStateAfterGoingHome.copy(path = Queue.empty))
    )

    val stateAfterProcessingMove = updatedStateAfterGoingHome.copy(
      path = Queue.empty
    )

    expectMsg(
      BehaviourTickProcessed(
        stateAfterProcessingMove
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = stateAfterProcessingMove)
    )

    // at destination; no state update expected; transitioning to acting
    expectMsg(
      BehaviourTickProcessed(
        stateAfterProcessingMove
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = stateAfterProcessingMove)
    )

    // acting; NoAction is next on actions list; no state update expected
    expectMsg(
      BehaviourTickProcessed(
        stateAfterProcessingMove
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = stateAfterProcessingMove)
    )

    // no actions remaining; should destroy self
    expectMsg(
      ForwardMessage(DestroyEntity(Fixtures.MockRefs.walker))
    )

    // destroying; no state update expected
    expectMsg(
      BehaviourTickProcessed(
        stateAfterProcessingMove
      )
    )
  }

  it should "destroy itself when it has insufficient life" in { _ =>
    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testActor,
        Props(
          new BaseWalker {
            override protected def behaviour: Behaviour = idling()
          }
        )
      )
    )

    val idlingWalkerData = WalkerData(
      properties = Fixtures.Walker.properties,
      state = Fixtures.Walker.state.copy(
        currentLife = Life(0),
        commodities = CommoditiesState(
          available = Map(Commodity("TestCommodity") -> Commodity.Amount(200)),
          limits = Map.empty
        )
      ),
      modifiers = Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = idlingWalkerData
    )

    // no state update expected when destroying
    expectMsg(
      BehaviourTickProcessed(
        idlingWalkerData.state
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
      ForwardMessage(DestroyEntity(idlingWalkerData.id))
    )

    val walkerData = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    // should ignore instructions
    parentEntity ! ApplyInstructions(walkerData, instructions = Seq(new ActiveEntity.Instruction {}))
    expectMsg(InstructionsApplied())

    // should ignore messages
    parentEntity ! ApplyMessages(
      entity = walkerData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Walker.state
      )
    )

    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = idlingWalkerData
    )

    // no state update expected when destroying
    expectMsg(
      BehaviourTickProcessed(
        idlingWalkerData.state
      )
    )
  }

  it should "accept player instructions" in { _ =>
    val parentEntity = system.actorOf(
      WalkerParentEntity.props(
        testActor,
        Props(
          new BaseWalker {
            override protected def behaviour: Behaviour = idling()
          }
        )
      )
    )

    val walkerData = WalkerData(
      Fixtures.Walker.properties,
      Fixtures.Walker.state,
      Fixtures.Walker.modifiers,
      Fixtures.MockRefs.walker
    )

    // should process instructions
    parentEntity ! ApplyInstructions(
      walkerData,
      instructions = Seq(
        DoTransition(Advance(destination = DestinationPoint(2, 2), destinationActions = Seq.empty))
      )
    )
    expectMsg(InstructionsApplied())

    // should process messages
    parentEntity ! ApplyMessages(
      entity = walkerData,
      messages = Seq(
        ProcessCommodities(Seq((Commodity("TestCommodity"), Commodity.Amount(10))))
      )
    )

    expectMsg(
      MessagesApplied(
        Fixtures.Walker.state.copy(
          commodities = CommoditiesState(
            available = Map(Commodity("TestCommodity") -> Commodity.Amount(10)),
            limits = Map(Commodity("TestCommodity") -> Commodity.Amount(100))
          )
        )
      )
    )

    // is in idling state; cannot attack and should remain idling
    parentEntity ! ProcessBehaviourTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties.copy(attack = NoAttack),
        Fixtures.Walker.state,
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      )
    )

    receiveOne(max = 3.seconds) match {
      case ForwardMessage(MoveEntity(_, cell)) => cell should be(Point(2, 2))
      case message                             => fail(s"Unexpected message received: [$message]")
    }

    expectMsg(
      BehaviourTickProcessed(
        Fixtures.Walker.state.copy(
          distanceCovered = Distance(1)
        )
      )
    )
  }

  it should "not pass through roadblocks when appropriate" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not pass through enemy units" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
