package bfa

import scala.collection.mutable

object util {
  def memoize[I, O](f: I => O): I => O =
    new mutable.HashMap[I, O] with (I => O)() {
      override def apply(key: I) = getOrElseUpdate(key, f(key))
    }
}
