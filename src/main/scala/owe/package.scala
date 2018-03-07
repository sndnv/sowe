import java.util.UUID

package object owe {
  type Desirability = Int

  object Desirability {
    val Min: Desirability = -8
    val Max: Desirability = 8
    val Neutral: Desirability = 0
  }

  type EntityDesirability = (
    Desirability,
    Desirability,
    Desirability,
    Desirability,
    Desirability,
    Desirability
  )

  object EntityDesirability {
    val Min: EntityDesirability = (
      Desirability.Min,
      Desirability.Min,
      Desirability.Min,
      Desirability.Min,
      Desirability.Min,
      Desirability.Min
    )

    val Max: EntityDesirability = (
      Desirability.Max,
      Desirability.Max,
      Desirability.Max,
      Desirability.Max,
      Desirability.Max,
      Desirability.Max
    )

    val Neutral: EntityDesirability = (
      Desirability.Neutral,
      Desirability.Neutral,
      Desirability.Neutral,
      Desirability.Neutral,
      Desirability.Neutral,
      Desirability.Neutral
    )
  }

  type EntityID = UUID
  type EffectID = UUID

  /**
    * Parent trait for all messages used by the engine.
    */
  trait Message
}
