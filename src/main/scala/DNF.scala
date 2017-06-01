package bfa

/**
 * DNF provides Disjunctive Normal Form utilities.
 */
object DNF {
  /**
   * Create a new DNF (OrSet)
   *
   * @param dnf a set of set of symbols
   */
  def apply(dnf: Set[(Set[Symbol], Set[Symbol])]): OrSet =
    OrSet(dnf map { case (ts, fs) => AndSet(ts, fs) })

  /**
   * AndSet is a tuple of two set:
   *
   *   - first set contains '''and''' clause of symbols,
   *   - second set conatins '''and''' clause of '''not''' symbols.
   *
   * {{{
   * // below means A ∧ ¬B
   * AndSet(Set('A), Set('B))
   *
   * // below means A ∧ B ∧ ¬C ∧ ¬D
   * AndSet(Set('A, 'B), Set('C, 'D))
   * }}}
   */
  final case class AndSet(trues: Set[Symbol], falses: Set[Symbol]) {
    /**
     * Evaluate this AndSet with given set.
     *
     * @param trues symbols set to replace as true
     * @return an evaluation result using given set
     */
    def evaluate(trues: Set[Symbol]): Boolean =
      (this.trues subsetOf trues) && (this.falses & trues).isEmpty
  }

  /**
   * OrSet is DNF, which is a set of set of symbols.
   */
  final case class OrSet(andSets: Set[AndSet]) {
    /**
     * Evaluate this OrSet with given set.
     *
     * @param trues symbols to replace as true in this DNF
     * @return an evaluation result using given symbols
     */
    def evaluate(trues: Set[Symbol]): Boolean =
      andSets.exists(_.evaluate(trues))
  }
}
