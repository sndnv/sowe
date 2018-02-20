package owe.test.specs.unit.map

import org.scalatest.Outcome
import owe.test.specs.unit.UnitSpec

class GridSpec extends UnitSpec {
  case class FixtureParam()

  def withFixture(test: OneArgTest): Outcome = {
    withFixture(test.toNoArgTest(FixtureParam()))
  }

  "A Grid" should "add entities to cell" in { _ =>
    //TODO
  }

  it should "remove entities from cell" in { _ =>
    //TODO
  }

  it should "not add overlapping entities" in { _ =>
    //TODO
  }

  it should "remove entities from all cells they use" in { _ =>
    //TODO
  }

  it should "not remove entities not in cell" in { _ =>
    //TODO
  }

  it should "not remove entities when invalid id supplied" in { _ =>
    //TODO
  }

  it should "not add entities outside of bounds" in { _ =>
    //TODO
  }

  it should "not remove entities outside of bounds" in { _ =>
    //TODO
  }

  it should "find named entities" in { _ =>
    //TODO
  }

  it should "find closest named entity" in { _ =>
    //TODO
  }

  it should "find first adjacent entity of type" in { _ =>
    //TODO
  }

  "A Grid without entities" should "report correct cell neighbors" in { _ =>
    //TODO
  }

  it should "calculate paths between cells" in { _ =>
    //TODO
  }

  "A Grid with entities" should "report correct cell neighbors" in { _ =>
    //TODO
  }

  it should "calculate paths between cells" in { _ =>
    //TODO
  }
}
