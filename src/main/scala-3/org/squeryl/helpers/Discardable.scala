package org.squeryl.helpers

opaque type Discardable[A] <: A = A

object Discardable {

  given [A]: Conversion[A, Discardable[A]] = (x: A) => x

  def apply[A](x: A): Discardable[A] = x
}
