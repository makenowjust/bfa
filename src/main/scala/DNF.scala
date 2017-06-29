package bfa

/**
  * DNF provides Disjunctive Normal Form utilities.
  */
object DNF {

  /**
    * Create a new DNF (OrSet)
    *
    * @param dnf a set of tuple of set of symbols
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
      * Is this set empty?
      */
    def isEmpty: Boolean =
      this.trues.size == 0 && this.falses.size == 0

    /**
      * Is this set a singleton?
      */
    def isSingleton: Boolean =
      this.trues.size == 0 && this.falses.size == 1 ||
        this.trues.size == 1 && this.falses.size == 0

    /**
      * Evaluate this AndSet with given set.
      *
      * @param trues symbols set to replace as true
      * @return an evaluation result using given set
      */
    def evaluate(trues: Set[Symbol]): Boolean =
      (this.trues subsetOf trues) && (this.falses & trues).isEmpty

    /**
      * Invert this AndSet by using De Morgan's law.
      */
    def invert: OrSet =
      OrSet(
        this.trues.map { s =>
          AndSet(Set.empty, Set(s))
        } |
          this.falses.map { s =>
            AndSet(Set(s), Set.empty)
          })

    /**
      * Concat this and other AndSet.
      */
    def concat(other: AndSet): AndSet =
      AndSet(this.trues | other.trues, this.falses | other.falses)

    override def toString: String = {
      if (this.isEmpty) {
        "1"
      } else {
        val ts = this.trues.toArray.map { s =>
          s.name
        }
        val fs = this.falses.toArray.map { s =>
          s"¬${s.name}"
        }
        (ts ++ fs).mkString(" ∧ ")
      }
    }
  }

  object AndSet {

    /**
      * Create an empty AndSet.
      */
    def empty: AndSet = AndSet(Set.empty, Set.empty)
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
      this.andSets.exists(_.evaluate(trues))

    /**
      * Invert this OrSet using De Morgan's law.
      */
    def invert: OrSet =
      OrSet(
        this.andSets
          .map { a =>
            a.invert
          }
          .foldLeft(Set(AndSet.empty)) {
            case (as, OrSet(as2)) =>
              for {
                a <- as
                a2 <- as2
              } yield a.concat(a2)
          })

    override def toString: String =
      if (this.andSets.isEmpty) {
        "0"
      } else {
        this.andSets
          .map { as =>
            if (as.isEmpty || as.isSingleton) as.toString else s"($as)"
          }
          .mkString(" ∨ ")
      }
  }
}
