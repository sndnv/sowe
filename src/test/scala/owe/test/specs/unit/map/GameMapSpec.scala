package owe.test.specs.unit.map

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class GameMapSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome =
    withFixture(test.toNoArgTest(FixtureParam()))

  "A GameMap" should "add entities to cell" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "remove entities from cell" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not add overlapping entities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "remove entities from all cells they use" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not remove entities not in cell" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not remove entities when invalid id supplied" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not add entities outside of bounds" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "not remove entities outside of bounds" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "find named entities" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "find closest named entity" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "find first adjacent entity of type" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  "A GameMap without entities" should "report correct cell neighbors" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "calculate paths between cells" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  "A GameMap with entities" should "report correct cell neighbors" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }

  it should "calculate paths between cells" in { _ =>
    fail("Not Implemented", new NotImplementedError())
  }
}
