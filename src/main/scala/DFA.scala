package bfa

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
}

object DFA {
  def from(mbfa: MBFA): DFA = mbfa.toDFA
}
