package bfa

import collection.mutable

final case class DFA(
    init: Symbol,
    trans: Map[(Symbol, Char), Symbol],
    last: Set[Symbol],
) {
  def matches(s: String): Boolean =
    s.foldLeft(Option(init)) {
        case (o, c) =>
          o.flatMap { q =>
            trans.get((q, c))
          }
      }
      .map { q =>
        last.contains(q)
      }
      .getOrElse(false)

  def symbols: Set[Symbol] =
    Set(init) | trans.iterator.map { case ((k, _), v) => Set(k, v) }.toSet.flatten | last

  def toRegExp: AST = {
    import AST._

    val vs = symbols.toArray

    val deltaR = {
      val mm = new mutable.HashMap[(Symbol, Symbol), mutable.Set[AST]]
      with mutable.MultiMap[(Symbol, Symbol), AST]
      trans.foreach { case ((k, c), v) => mm.addBinding((k, v), Literal(c)) }
      mm.map { case ((k, v), cs) => (k, v) -> cs.toList.reduceLeft(_ | _) }.toMap
    }

    lazy val r: ((Int, Int, Int)) => AST = util.memoize {
      case (i, j, k) =>
        k match {
          case -1 if i == j => deltaR.getOrElse((vs(i), vs(j)), Fail).?
          case -1 if i != j => deltaR.getOrElse((vs(i), vs(j)), Fail)
          case k =>
            r(i, j, k - 1) | (r(i, k, k - 1) ~ r(k, k, k - 1).* ~ r(k,
                                                                    j,
                                                                    k - 1))
        }
    }

    val i = vs.indexOf(init)
    last.iterator
      .map { v =>
        r(i, vs.indexOf(v), vs.length - 1)
      }
      .reduceLeftOption(_ | _)
      .getOrElse(Fail)
  }
}

object DFA {
  def from(mbfa: MBFA): DFA = mbfa.toDFA
}
