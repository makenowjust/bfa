package com.github.makenowjust.bfa

final case class DFA(
  Q: Set[Symbol],
  Σ: Set[Char],
  δ: Map[(Symbol, Char), Symbol],
  q0: Symbol,
  F: Set[Symbol]
) {
  def apply(w: String): Boolean =
    (Option(q0) /: w.toSeq) { (q, a) =>
      q.flatMap(q => δ.get((q, a)))
    }.fold(false) { F.contains(_) }
}
