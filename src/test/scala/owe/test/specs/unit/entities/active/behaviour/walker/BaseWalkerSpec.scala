package owe.test.specs.unit.entities.active.behaviour.walker

import akka.actor.Props
import akka.testkit.TestProbe
import org.scalatest.Outcome
import owe.entities.ActiveEntity.{ForwardMessage, ProcessEntityTick, WalkerData}
import owe.entities.active.Structure.StructureRef
import owe.entities.active._
import owe.entities.active.Walker._
import owe.entities.active.behaviour.UpdateExchange
import owe.entities.active.behaviour.walker.BaseWalker
import owe.entities.active.behaviour.walker.BaseWalker._
import owe.map.GameMap.{AttackEntity, DestroyEntity, ForwardExchangeMessage, MoveEntity}
import owe.map.grid.Point
import owe.production.Commodity
import owe.production.Exchange.UpdateCommodityState
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.entities.active.behaviour.{Fixtures, WalkerParentEntity}

import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.concurrent.duration._

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

    // is in attacking state; should process attack
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state,
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      ),
      messages = Seq.empty
    )

    val damage = receiveOne(max = 3.seconds).asInstanceOf[ForwardMessage] match {
      case ForwardMessage(attack: AttackEntity) => attack.damage
      case message                              => fail(s"Unexpected message received: [$message]")
    }

    damage should be(AttackDamage(25))

    expectMsg(
      Fixtures.Walker.state
    )

    // is in attacking state; without target, should not attack and should switch to idling
    parentEntity ! ProcessEntityTick(
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
      ),
      messages = Seq.empty
    )

    expectMsg(
      Fixtures.Walker.state
    )

    // is in idling state; should not attack
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties.copy(attack = NoAttack),
        Fixtures.Walker.state,
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      ),
      messages = Seq.empty
    )

    expectMsg(
      Fixtures.Walker.state.copy(
        mode = MovementMode.Idling
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

    // is in idling state; cannot attack and should remain idling
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties.copy(attack = NoAttack),
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      ),
      messages = Seq.empty
    )

    expectMsg(
      Fixtures.Walker.state.copy(
        mode = MovementMode.Idling
      )
    )

    // is in idling state; can attack and should switch to attacking state
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      ),
      messages = Seq.empty
    )

    expectMsg(
      Fixtures.Walker.state.copy(
        mode = MovementMode.Idling
      )
    )

    // is in attacking state and should process attack
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      ),
      messages = Seq.empty
    )

    val damage = receiveOne(max = 3.seconds).asInstanceOf[ForwardMessage] match {
      case ForwardMessage(attack: AttackEntity) => attack.damage
      case message                              => fail(s"Unexpected message received: [$message]")
    }

    damage should be(AttackDamage(25))

    expectMsg(
      Fixtures.Walker.state.copy(
        mode = MovementMode.Idling
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

    val roamingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(0, 0),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack
      ),
      state = State(
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue(Point(0, 1), Point(0, 2)),
        mode = MovementMode.Roaming
      ),
      modifiers = StateModifiers(
        movementSpeed = SpeedModifier(100),
        maxRoamingDistance = DistanceModifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    // is in roaming state; cannot attack, should remain roaming and move to next position
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData,
      messages = Seq.empty
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

    expectMsg(updatedStateWithMovement)

    // is in roaming state; cannot attack, should remain roaming and move to next position
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData.copy(state = updatedStateWithMovement),
      messages = Seq.empty
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

    expectMsg(updatedStateWithPathComplete)

    // is in roaming state; cannot attack; current roaming path complete; generate new roaming path
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData.copy(state = updatedStateWithPathComplete),
      messages = Seq.empty
    )

    val updatedStateWithNewRoamingPath = roamingWalkerData.state.copy(
      distanceCovered = Distance(2),
      path = Queue(Point(0, 1), Point(0, 2), Point(1, 2))
    )

    expectMsg(updatedStateWithNewRoamingPath)

    // is in roaming state; cannot attack; current roaming path complete; generate new roaming path
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData.copy(state = updatedStateWithNewRoamingPath),
      messages = Seq.empty
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

    expectMsg(updatedStateWithUsingNewRoamingPath)

    // is in roaming state; should not move to next position (no free path)
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = roamingWalkerData.copy(
        properties = roamingWalkerData.properties.copy(
          maxRoamingDistance = Distance(2)
        ),
        state = roamingWalkerData.state.copy(
          distanceCovered = Distance(1),
          path = Queue.empty
        )
      ),
      messages = Seq.empty
    )

    expectMsg(
      roamingWalkerData.state.copy(
        distanceCovered = Distance(1),
        path = Queue.empty,
        mode = MovementMode.Idling
      )
    )

    // is in roaming state; can attack and should switch to attacking state
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state.copy(
          mode = MovementMode.Roaming
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      ),
      messages = Seq.empty
    )

    expectMsg(Fixtures.Walker.state.copy(mode = MovementMode.Roaming))

    // is in attacking state and should process attack
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = WalkerData(
        Fixtures.Walker.properties,
        Fixtures.Walker.state.copy(
          mode = MovementMode.Idling
        ),
        Fixtures.Walker.modifiers,
        Fixtures.MockRefs.walker
      ),
      messages = Seq.empty
    )

    val damage = receiveOne(max = 3.seconds).asInstanceOf[ForwardMessage] match {
      case ForwardMessage(attack: AttackEntity) => attack.damage
      case message                              => fail(s"Unexpected message received: [$message]")
    }

    damage should be(AttackDamage(25))

    expectMsg(Fixtures.Walker.state.copy(mode = MovementMode.Idling))

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

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = returningEntityData,
      messages = Seq.empty
    )

    expectMsg(returningEntityData.state)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = returningEntityData,
      messages = Seq.empty
    )

    expectMsg(returningEntityData.state)
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

    val advancingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(1, 1),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack
      ),
      state = State(
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue(Point(2, 2)),
        mode = MovementMode.Advancing
      ),
      modifiers = StateModifiers(
        movementSpeed = SpeedModifier(100),
        maxRoamingDistance = DistanceModifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    // is in advancing state; is not at destination, should move
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 2)),
      entity = advancingWalkerData,
      messages = Seq.empty
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

    expectMsg(updatedStateWithMovement)

    // is in advancing state; is at destination, should switch to acting
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(2, 2)),
      entity = advancingWalkerData.copy(state = updatedStateWithMovement),
      messages = Seq.empty
    )

    expectMsg(updatedStateWithMovement)

    // is in acting state; is at destination, should switch to advancing (going home)
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(2, 2)),
      entity = advancingWalkerData.copy(state = updatedStateWithMovement),
      messages = Seq.empty
    )

    expectMsg(updatedStateWithMovement)

    // is in advancing state; is at destination, should be going home
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(2, 2)),
      entity = advancingWalkerData.copy(state = updatedStateWithMovement),
      messages = Seq.empty
    )

    val updatedStateGoingHome = updatedStateWithMovement.copy(
      path = Queue(Point(1, 0), Point(2, 0), Point(2, 1), Point(1, 1), Point(1, 1)),
      mode = MovementMode.Returning
    )

    expectMsg(updatedStateGoingHome)
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

    val advancingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(1, 1),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack
      ),
      state = State(
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue.empty,
        mode = MovementMode.Advancing
      ),
      modifiers = StateModifiers(
        movementSpeed = SpeedModifier(100),
        maxRoamingDistance = DistanceModifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    // is in advancing state; is at home and should get path to destination entity
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = advancingWalkerData,
      messages = Seq.empty
    )

    val updatedStateGoingToEntity = advancingWalkerData.state.copy(
      path = Queue(Point(1, 0), Point(2, 0), Point(2, 1), Point(1, 1), Point(0, 1)),
      mode = MovementMode.Advancing
    )

    expectMsg(updatedStateGoingToEntity)

    // is in advancing state; cannot proceed on path
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = advancingWalkerData.copy(state = advancingWalkerData.state.copy(path = Queue(Point(0, 0)))),
      messages = Seq.empty
    )

    expectMsg(updatedStateGoingToEntity)
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

    val advancingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(1, 1),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack
      ),
      state = State(
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue.empty,
        mode = MovementMode.Advancing
      ),
      modifiers = StateModifiers(
        movementSpeed = SpeedModifier(100),
        maxRoamingDistance = DistanceModifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    // is in advancing state; is at home and should fail get path to unreachable entity
    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = advancingWalkerData,
      messages = Seq.empty
    )

    val updatedStateWithNoPath = advancingWalkerData.state.copy(
      path = Queue.empty,
      mode = MovementMode.Idling
    )

    expectMsg(updatedStateWithNoPath)
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

    val actingWalkerData = WalkerData(
      properties = Properties(
        parent = None,
        homePosition = Point(1, 1),
        name = "TestWalker",
        maxLife = Life(500),
        movementSpeed = Speed(1),
        maxRoamingDistance = Distance(50),
        attack = NoAttack
      ),
      state = State(
        currentLife = Life(100),
        distanceCovered = Distance(0),
        commodities = NoCommodities,
        path = Queue.empty,
        mode = MovementMode.Idling
      ),
      modifiers = StateModifiers(
        movementSpeed = SpeedModifier(100),
        maxRoamingDistance = DistanceModifier(100),
        attack = NoAttack
      ),
      Fixtures.MockRefs.walker
    )

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData,
      messages = Seq.empty
    )

    val updatedStateAfterDoOperation = actingWalkerData.state.copy(
      currentLife = Life(42)
    )

    expectMsg(updatedStateAfterDoOperation)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData.copy(state = updatedStateAfterDoOperation),
      messages = Seq.empty
    )

    val updatedStateAfterOneRepeatableOperation = actingWalkerData.state.copy(
      currentLife = Life(5042)
    )

    expectMsg(updatedStateAfterOneRepeatableOperation)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData.copy(state = updatedStateAfterOneRepeatableOperation),
      messages = Seq.empty
    )

    val updatedStateAfterTwoRepeatableOperations = actingWalkerData.state.copy(
      currentLife = Life(10042)
    )

    expectMsg(updatedStateAfterTwoRepeatableOperations)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData.copy(state = updatedStateAfterTwoRepeatableOperations),
      messages = Seq.empty
    )

    // repeatable operation not applicable; no state update is expected
    expectMsg(updatedStateAfterTwoRepeatableOperations)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = actingWalkerData.copy(state = updatedStateAfterTwoRepeatableOperations),
      messages = Seq.empty
    )

    // just transitioned to advancing; no state update is expected
    expectMsg(updatedStateAfterTwoRepeatableOperations)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(
        state = updatedStateAfterTwoRepeatableOperations.copy(path = Queue.empty)
      ),
      messages = Seq.empty
    )

    val updatedStateAfterGoingToPoint = updatedStateAfterTwoRepeatableOperations.copy(
      path = Queue.empty
    )

    expectMsg(updatedStateAfterGoingToPoint)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = updatedStateAfterGoingToPoint.copy(path = Queue.empty)),
      messages = Seq.empty
    )

    val updatedStateAfterGoingToEntity = updatedStateAfterGoingToPoint.copy(
      path = Queue.empty
    )

    expectMsg(updatedStateAfterGoingToEntity)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(0, 0)),
      entity = actingWalkerData.copy(state = updatedStateAfterGoingToEntity),
      messages = Seq.empty
    )

    val updatedStateAfterGoingHome = updatedStateAfterGoingToEntity.copy(
      path = Queue.empty
    )

    expectMsg(updatedStateAfterGoingHome)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = updatedStateAfterGoingHome.copy(path = Queue.empty)),
      messages = Seq.empty
    )

    val stateAfterProcessingMove = updatedStateAfterGoingHome.copy(
      path = Queue.empty
    )

    expectMsg(stateAfterProcessingMove)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = stateAfterProcessingMove),
      messages = Seq.empty
    )

    // at destination; no state update expected; transitioning to acting
    expectMsg(stateAfterProcessingMove)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = stateAfterProcessingMove),
      messages = Seq.empty
    )

    // acting; NoAction is next on actions list; no state update expected
    expectMsg(stateAfterProcessingMove)

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData.copy(position = Point(1, 1)),
      entity = actingWalkerData.copy(state = stateAfterProcessingMove),
      messages = Seq.empty
    )

    // acting on empty actions list; no state update exepcted
    expectMsg(stateAfterProcessingMove)
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

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = idlingWalkerData,
      messages = Seq.empty
    )

    // no state update expected when destroying
    expectMsg(
      idlingWalkerData.state
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

    parentEntity ! ProcessEntityTick(
      map = Fixtures.defaultMapData,
      entity = idlingWalkerData,
      messages = Seq.empty
    )

    // no state update expected when destroying
    expectMsg(
      idlingWalkerData.state
    )
  }

  it should "accept player instructions" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not pass through roadblocks when appropriate" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not pass through enemy units" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "halt movement if target cannot accept resources" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
