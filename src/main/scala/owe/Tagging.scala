package owe

// based on - http://www.edofic.com/posts/2015-05-02-tagged-types.html
object Tagging {
  trait Tagged[+V, +T]
  type @@[+V, +T] = V with Tagged[V, T]

  implicit class Taggable[V](val value: V) extends AnyVal {
    def tag[T]: @@[V, T] = value.asInstanceOf[V @@ T]
  }
}
