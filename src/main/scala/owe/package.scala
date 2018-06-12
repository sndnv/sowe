import java.util.UUID

package object owe {
  final case class CellDesirability(value: Int) extends AnyVal {
    def +(desirability: CellDesirability): CellDesirability = CellDesirability(value + desirability.value)
    def -(desirability: CellDesirability): CellDesirability = CellDesirability(value - desirability.value)
    def >(desirability: CellDesirability): Boolean = value > desirability.value
    def <(desirability: CellDesirability): Boolean = value < desirability.value
    def >=(desirability: CellDesirability): Boolean = value >= desirability.value
    def <=(desirability: CellDesirability): Boolean = value <= desirability.value
    def min(desirability: CellDesirability): CellDesirability = CellDesirability(value.min(desirability.value))
    def max(desirability: CellDesirability): CellDesirability = CellDesirability(value.max(desirability.value))
  }

  final case class CellDesirabilityModifier(value: Int) extends AnyVal {
    def apply(desirability: CellDesirability): CellDesirability =
      CellDesirability((desirability.value * value) / 100)
  }

  object CellDesirability {
    val Min: CellDesirability = CellDesirability(-8)
    val Max: CellDesirability = CellDesirability(8)
    val Neutral: CellDesirability = CellDesirability(0)
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
    def fromInt(
      self: Int,
      r1: Int,
      r2: Int,
      r3: Int,
      r4: Int,
      r5: Int
    ): EntityDesirability = new EntityDesirability(
      CellDesirability(self),
      CellDesirability(r1),
      CellDesirability(r2),
      CellDesirability(r3),
      CellDesirability(r4),
      CellDesirability(r5)
    )

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

  final case class Fertility(value: Int) extends AnyVal {
    def min(fertility: Fertility): Fertility = Fertility(value.min(fertility.value))
    def max(fertility: Fertility): Fertility = Fertility(value.max(fertility.value))
    def basedOn(water: Water): Fertility = Fertility((water.value * value) / 100)
  }

  final case class FertilityModifier(value: Int) extends AnyVal {
    def apply(fertility: Fertility): Fertility = Fertility((fertility.value * value) / 100)
  }

  object Fertility {
    val Min = Fertility(0)
    val Max = Fertility(100)
  }

  final case class Water(value: Int) extends AnyVal {
    def min(water: Water): Water = Water(value.min(water.value))
    def max(water: Water): Water = Water(value.max(water.value))
  }

  final case class WaterModifier(value: Int) extends AnyVal {
    def apply(water: Water): Water = Water((water.value * value) / 100)
  }

  object Water {
    val Max = Water(100)
    val Min = Water(0)
  }

  type EntityID = UUID
  type EffectID = UUID

  /**
    * Parent trait for all messages used by the engine.
    */
  trait Message
}
