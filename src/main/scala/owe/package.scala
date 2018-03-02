import java.util.UUID

package object owe {
  type Desirability = Int

  object Desirability {
    val Min: Desirability = -8
    val Max: Desirability = 8
    val Neutral: Desirability = 0
  }

  type EntityID = UUID
  type EffectID = UUID

  /**
    * Parent trait for all messages used by the engine.
    */
  trait Message
}
