package owe.test.specs.unit.map

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class GameMapSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A GameMap" should "process game ticks when active" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "respond with advance paths when active" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "respond with roaming paths when active" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "respond with neighbours when active" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "respond with multiple entities when active" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "respond with single entities when active" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "complete a game tick when active" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "stash all unsupported messages when active" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "update entities when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "create entities when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "destroy entities when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "move entities when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "process commodity distribution when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "process entity attacks when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "process labour found updates when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "process occupants updates when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "process labour updates when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "forward messages to commodity exchange when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "start tick processing when idle" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
