import java.util.UUID

package object owe {
  type EffectID = UUID

  /**
    * Parent trait for all messages used by the engine.
    */
  trait Message
}
