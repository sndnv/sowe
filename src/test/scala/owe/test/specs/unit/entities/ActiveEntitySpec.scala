package owe.test.specs.unit.entities

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.testkit.TestProbe
import akka.util.Timeout
import org.scalatest.Outcome
import owe.effects.Effect
import owe.entities.ActiveEntity
import owe.entities.ActiveEntity.{MapData, ResourceData, StructureData, WalkerData}
import owe.entities.ActiveEntityActor._
import owe.entities.Entity.{ProcessAttack, ProcessLabourFound, ProcessLabourUpdate}
import owe.entities.active.Resource.{Properties, ResourceRef, State, StateModifiers}
import owe.entities.active.Structure._
import owe.entities.active.Walker.{MovementMode, NoAttack, TraversalMode, WalkerRef}
import owe.entities.active.attributes._
import owe.entities.active.behaviour.resource.BaseResource
import owe.entities.active.{Resource, Structure, Walker}
import owe.map.Cell
import owe.map.GameMap.{EntityTickProcessed, LabourFound}
import owe.map.grid.Point
import owe.production.Commodity
import owe.test.specs.unit.AkkaUnitSpec
import owe.test.specs.unit.effects.definitions.{DecreaseFireRisk, IncreaseMovementSpeed, StopProduction}
import owe.test.specs.unit.entities.TestParentMap.ParentMapMessage

import scala.collection.immutable.Queue
import scala.concurrent.Await
import scala.concurrent.duration._

class ActiveEntitySpec extends AkkaUnitSpec("ActiveEntitySpec") {
  private implicit val timeout: Timeout = 3.seconds

  private val mapData = MapData(position = (0, 0), cellState = Cell.CellData.empty.state)

  private val entityProperties = Properties(
    name = "TestResource",
    homePosition = Point(0, 0),
    commodity = Commodity("TestCommodity"),
    maxAmount = Commodity.Amount(500)
  )

  private val entityState = State(
    currentAmount = Commodity.Amount(100),
    replenishAmount = Commodity.Amount(25)
  )

  private val entityModifiers = StateModifiers(
    replenishAmount = Commodity.AmountModifier(100)
  )

  private class TestEntity(ref: ActorRef) extends Resource {
    override protected def createActiveEntityData(): ActiveEntity.ActiveEntityRef => ActiveEntity.Data = {
      case resourceRef: ResourceRef =>
        ResourceData(
          properties = entityProperties,
          state = entityState,
          modifiers = entityModifiers,
          id = resourceRef
        )
    }

    override protected def createEffects(): Seq[(ActiveEntity.Data => Boolean, Effect)] =
      Seq(
        ((_: ActiveEntity.Data) => false, new DecreaseFireRisk),
        ((_: ActiveEntity.Data) => true, new IncreaseMovementSpeed),
        ((_: ActiveEntity.Data) => true, new StopProduction)
      )

    override protected def createBehaviour(): BaseResource = new BaseResource {
      private def forwardDestroy: Behaviour = {
        case destroy: DestroyBehaviour =>
          ref.forward(destroy)
          context.become(destroying())
      }

      override protected def base: Behaviour = forwardDestroy.orElse(behaviour)

      override protected def behaviour: Behaviour = {
        case message => ref.forward(message)
      }
    }
  }

  case class FixtureParam(testProbe: TestProbe, mapActor: ActorRef, entityActor: ActorRef)

  def withFixture(test: OneArgTest): Outcome = {
    val testProbe = TestProbe()
    val entity = new TestEntity(testProbe.ref)
    val mapActor = system.actorOf(Props(new TestParentMap(testProbe.ref, entity.props())))
    val entityActor = Await.result((mapActor ? TestParentMap.GetEntity()).mapTo[ActorRef], timeout.duration)

    withFixture(test.toNoArgTest(FixtureParam(testProbe, mapActor, entityActor)))
  }

  "An Active Entity" should "update entity state when active" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ApplyMessages]

    fixture.entityActor.tell(MessagesApplied(entityState), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ProcessBehaviourTick]

    fixture.entityActor.tell(GetData(), fixture.testProbe.ref)
    fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[ResourceData].state should be(entityState)

    val updatedState = entityState.copy(currentAmount = Commodity.Amount(42))
    fixture.entityActor.tell(BehaviourTickProcessed(updatedState), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(ParentMapMessage(EntityTickProcessed(tick = 0)))
    fixture.entityActor.tell(GetData(), fixture.testProbe.ref)
    fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[ResourceData].state should be(updatedState)
  }

  it should "forward messages to parent when active" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ApplyMessages]

    fixture.entityActor.tell(MessagesApplied(entityState), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ProcessBehaviourTick]

    val parentMapMessage = LabourFound(StructureRef(TestProbe().ref))
    fixture.entityActor.tell(ForwardMessage(parentMapMessage), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(ParentMapMessage(parentMapMessage))
  }

  it should "respond with entity data when active" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ApplyMessages]

    fixture.entityActor.tell(MessagesApplied(entityState), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ProcessBehaviourTick]

    fixture.entityActor.tell(GetData(), fixture.testProbe.ref)
    fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[ResourceData].state should be(entityState)
  }

  it should "stash unsupported messages when active" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ApplyMessages]

    fixture.entityActor.tell(MessagesApplied(entityState), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ProcessBehaviourTick]

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.expectNoMessage()

    // entity becomes idle after a state update and should process next game tick request
    fixture.entityActor.tell(BehaviourTickProcessed(entityState), fixture.testProbe.ref)
    val result = fixture.testProbe.receiveWhile(timeout.duration) {
      case ParentMapMessage(EntityTickProcessed(tick)) => tick
      case ApplyMessages(_, _)                         => 0
    }

    result should be(Seq(0, 0))
  }

  it should "stash unsupported messages when processing messages" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ApplyMessages]

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.expectNoMessage()

    fixture.entityActor.tell(MessagesApplied(entityState), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ProcessBehaviourTick]
    fixture.testProbe.expectNoMessage()

    // entity becomes idle after a state update and should process next game tick request
    fixture.entityActor.tell(BehaviourTickProcessed(entityState), fixture.testProbe.ref)
    val result = fixture.testProbe.receiveWhile(timeout.duration) {
      case ParentMapMessage(EntityTickProcessed(tick)) => tick
      case ApplyMessages(_, _)                         => 0
    }

    result should be(Seq(0, 0))
  }

  it should "apply entity effects when idle" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    val testEffect1 = new ActiveEntity.Effect[Resource.Properties, Resource.State, Resource.StateModifiers] {
      override def apply(entityData: ActiveEntity.Data): StateModifiers =
        entityData match {
          case ResourceData(_, _, modifiers, _) =>
            modifiers.copy(replenishAmount = Commodity.AmountModifier(5))

          case data => fail(s"Unexpected entity data found: [$data]")
        }

      override def radius: Int = 1
    }

    val testEffect2 = new ActiveEntity.Effect[Resource.Properties, Resource.State, Resource.StateModifiers] {
      override def apply(entityData: ActiveEntity.Data): StateModifiers =
        entityData match {
          case ResourceData(_, _, modifiers, _) =>
            modifiers.copy(replenishAmount = modifiers.replenishAmount * 7)

          case data => fail(s"Unexpected entity data found: [$data]")
        }

      override def radius: Int = 1
    }

    val testEffect3 = new Effect {
      override def radius: Int = 1
    }

    val testEffect4 = new ActiveEntity.Effect[Resource.Properties, Resource.State, Structure.StateModifiers] {
      override def apply(entityData: ActiveEntity.Data): Structure.StateModifiers =
        Structure.StateModifiers(
          risk = NoRisk,
          commodities = NoCommodities,
          housing = NoHousing,
          production = NoProduction
        )

      override def radius: Int = 1
    }

    fixture.entityActor.tell(
      ApplyEffects(Seq(testEffect1, testEffect2, testEffect3, testEffect4)),
      fixture.testProbe.ref
    )

    fixture.entityActor.tell(GetData(), fixture.testProbe.ref)
    val updatedModifiers = fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[ResourceData].modifiers
    updatedModifiers should be(StateModifiers(replenishAmount = Commodity.AmountModifier(35)))
  }

  it should "become active to process game ticks when idle" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[ApplyMessages]
  }

  it should "respond with active entity effects when idle" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(GetActiveEffects(), fixture.testProbe.ref)
    val effects = fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[Seq[Effect]]
    (effects should have).size(2)
    effects.exists(_.isInstanceOf[IncreaseMovementSpeed]) should be(true)
    effects.exists(_.isInstanceOf[StopProduction]) should be(true)
  }

  it should "respond with entity data when idle" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(GetData(), fixture.testProbe.ref)
    fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[ResourceData].state should be(entityState)
  }

  it should "forward messages to parent when idle" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    val parentMapMessage = LabourFound(StructureRef(TestProbe().ref))
    fixture.entityActor.tell(ForwardMessage(parentMapMessage), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(ParentMapMessage(parentMapMessage))
  }

  it should "add entity messages when idle" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    val messages = Seq(
      ProcessAttack(damage = AttackDamage(42)),
      ProcessLabourFound(),
      ProcessLabourUpdate(employees = 17)
    )

    messages.foreach(message => fixture.entityActor.tell(AddEntityMessage(message), fixture.testProbe.ref))
    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[ApplyMessages].messages should be(messages)
  }

  it should "add entity instructions when idle" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    val instructions = Seq(
      new ActiveEntity.Instruction {},
      new ActiveEntity.Instruction {},
      new ActiveEntity.Instruction {}
    )

    instructions.foreach { instruction =>
      fixture.entityActor.tell(AddEntityInstruction(instruction), fixture.testProbe.ref)
    }

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[ApplyInstructions].instructions should be(instructions)
    fixture.testProbe.expectMsgType[ApplyMessages]
  }

  it should "process destroy requests when idle" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(DestroySelf(), fixture.testProbe.ref)

    val testEntityData = fixture.testProbe.receiveOne(timeout.duration).asInstanceOf[DestroyBehaviour].entityData
    testEntityData.properties should be(entityProperties)
    testEntityData.state should be(entityState)
    testEntityData.modifiers should be(entityModifiers)
  }

  it should "accept destroy confirmation and stop itself and child behaviour when destroying" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    val observer = TestProbe()
    observer.watch(fixture.entityActor)

    fixture.entityActor.tell(DestroySelf(), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[DestroyBehaviour]

    fixture.entityActor.tell(BehaviourDestroyed(), fixture.testProbe.ref)
    observer.expectTerminated(fixture.entityActor, timeout.duration)
  }

  it should "forward messages to parent when destroying" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(DestroySelf(), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[DestroyBehaviour]

    val parentMapMessage = LabourFound(StructureRef(TestProbe().ref))
    fixture.entityActor.tell(ForwardMessage(parentMapMessage), fixture.testProbe.ref)
    fixture.testProbe.expectMsg(ParentMapMessage(parentMapMessage))
  }

  it should "ignore messages when destroying" in { fixture =>
    fixture.testProbe.expectMsgType[CreateBehaviour]

    fixture.entityActor.tell(DestroySelf(), fixture.testProbe.ref)
    fixture.testProbe.expectMsgType[DestroyBehaviour]

    fixture.entityActor.tell(ProcessEntityTick(tick = 0, mapData), fixture.testProbe.ref)
    fixture.testProbe.expectNoMessage(timeout.duration)
  }

  "Active Entity Resource Data" should "update its state and modifiers" in { _ =>
    val data = ResourceData(
      properties = entityProperties,
      state = entityState,
      modifiers = entityModifiers,
      id = ResourceRef(TestProbe().ref)
    )
    data.state should be(data.state)
    data.modifiers should be(data.modifiers)

    val updatedState = entityState.copy(currentAmount = Commodity.Amount(9000))
    data.withState(updatedState).state should be(updatedState)

    val updatedModifiers = entityModifiers.copy(replenishAmount = Commodity.AmountModifier(1))
    data.withModifiers(updatedModifiers).modifiers should be(updatedModifiers)
  }

  "Active Entity Structure Data" should "update its state and modifiers" in { _ =>
    val structureProperties = Structure.Properties(
      homePosition = Point(0, 0),
      name = "TestProducingStructure",
      walkers = NoWalkers,
      stages = SingleStage(
        stage = StageProperties(
          maxLife = Life(100),
          maxPeople = 15,
          minDesirability = Cell.Desirability.Neutral,
          commodityShortageLimit = 0
        )
      )
    )

    val structureState = Structure.State(
      risk = NoRisk,
      commodities = NoCommodities,
      housing = NoHousing,
      production = NoProduction,
      currentStage = DefaultStage,
      currentLife = Life(100),
      walkers = NoWalkers
    )

    val structureModifiers = Structure.StateModifiers(
      risk = NoRisk,
      commodities = NoCommodities,
      housing = NoHousing,
      production = NoProduction
    )

    val data = StructureData(
      properties = structureProperties,
      state = structureState,
      modifiers = structureModifiers,
      id = StructureRef(TestProbe().ref)
    )
    data.state should be(data.state)
    data.modifiers should be(data.modifiers)

    val updatedState = structureState.copy(currentLife = Life(50000))
    data.withState(updatedState).state should be(updatedState)

    val updatedModifiers = structureModifiers.copy(
      risk = RiskModifier(fire = RiskAmount(10), damage = RiskAmount(1))
    )
    data.withModifiers(updatedModifiers).modifiers should be(updatedModifiers)
  }

  "Active Entity Walker Data" should "update its state and modifiers" in { _ =>
    val walkerProperties = Walker.Properties(
      parent = None,
      homePosition = Point(0, 1),
      name = "TestWalker",
      maxLife = Life(500),
      movementSpeed = Speed(150),
      maxRoamingDistance = Distance(50),
      attack = NoAttack,
      traversalMode = TraversalMode.OnLand
    )

    val walkerState = Walker.State(
      currentPosition = Point(0, 1),
      currentLife = Life(100),
      distanceCovered = Distance(0),
      commodities = Walker.NoCommodities,
      path = Queue.empty,
      mode = MovementMode.Advancing
    )

    val walkerModifiers = Walker.StateModifiers(
      movementSpeed = Speed.Modifier(100),
      maxRoamingDistance = Distance.Modifier(100),
      attack = NoAttack
    )

    val data = WalkerData(
      properties = walkerProperties,
      state = walkerState,
      modifiers = walkerModifiers,
      id = WalkerRef(TestProbe().ref)
    )
    data.state should be(data.state)
    data.modifiers should be(data.modifiers)

    val updatedState = walkerState.copy(currentLife = Life(50000))
    data.withState(updatedState).state should be(updatedState)

    val updatedModifiers = walkerModifiers.copy(movementSpeed = Speed.Modifier(999))
    data.withModifiers(updatedModifiers).modifiers should be(updatedModifiers)
  }
}
