import java.util.UUID

package object owe {
  type CellDesirability = Int

  object CellDesirability {
    val Min: CellDesirability = -8
    val Max: CellDesirability = 8
    val Neutral: CellDesirability = 0
  }

  case class EntityDesirability(
    self: CellDesirability,
    r1: CellDesirability,
    r2: CellDesirability,
    r3: CellDesirability,
    r4: CellDesirability,
    r5: CellDesirability
  ) {
    def +(that: EntityDesirability): EntityDesirability =
      EntityDesirability(
        this.self + that.self,
        this.r1 + that.r1,
        this.r2 + that.r2,
        this.r3 + that.r3,
        this.r4 + that.r4,
        this.r5 + that.r5
      )

    def -(that: EntityDesirability): EntityDesirability =
      EntityDesirability(
        this.self - that.self,
        this.r1 - that.r1,
        this.r2 - that.r2,
        this.r3 - that.r3,
        this.r4 - that.r4,
        this.r5 - that.r5
      )

    def toMap: Map[Int, CellDesirability] = Map(
      0 -> self,
      1 -> r1,
      2 -> r2,
      3 -> r3,
      4 -> r4,
      5 -> r5
    )
  }

  object EntityDesirability {
    val Min: EntityDesirability = EntityDesirability(
      CellDesirability.Min,
      CellDesirability.Min,
      CellDesirability.Min,
      CellDesirability.Min,
      CellDesirability.Min,
      CellDesirability.Min
    )

    val Max: EntityDesirability = EntityDesirability(
      CellDesirability.Max,
      CellDesirability.Max,
      CellDesirability.Max,
      CellDesirability.Max,
      CellDesirability.Max,
      CellDesirability.Max
    )

    val Neutral: EntityDesirability = EntityDesirability(
      CellDesirability.Neutral,
      CellDesirability.Neutral,
      CellDesirability.Neutral,
      CellDesirability.Neutral,
      CellDesirability.Neutral,
      CellDesirability.Neutral
    )
  }

  type EntityID = UUID
  type EffectID = UUID

  /**
    * Parent trait for all messages used by the engine.
    */
  trait Message
}
