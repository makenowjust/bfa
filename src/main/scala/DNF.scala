package bfa

/**
  * DNF provides Disjunctive Normal Form utilities.
  */
object DNF {

  /**
    * Convert AndSet into OrSet (for DSL).
    *
    * @param andSets an AndSet converted into OrSet
    */
  def apply(andSet: AndSet): OrSet = OrSet(Set(andSet))

  /**
    * Identity (for DSL).
    *
    * @param orSet same as the result.
    */
  def apply(orSet: OrSet): OrSet = orSet

  /**
    * Create a singleton (for DSL).
    */
  def symbol(s: Symbol): AndSet = AndSet(Set(s), Set.empty)

  /**
    * Create a not-version singleton (for DSL).
    */
  def not(s: Symbol): AndSet = AndSet(Set.empty, Set(s))

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
        this.trues.map(DNF.not)
          | this.falses.map(DNF.symbol))

    /**
      * Concat this and other AndSet.
      */
    def concat(other: AndSet): AndSet =
      AndSet(this.trues | other.trues, this.falses | other.falses)

    /**
      * Replace symbol by given map.
      */
    def replace(map: Symbol => Option[OrSet]): OrSet = {
      val ass1 = this.trues
        .foldLeft(List(DNF.`1`)) { (oas, t) =>
          for {
            as1 <- oas
            as2 <- map(t).getOrElse(DNF.`0`).andSets
          } yield as1 ∧ as2
        }
        .toSet
      val ass2 = this.falses.foldLeft(ass1) { (ass, t) =>
        for {
          as1 <- ass
          as2 <- map(t).getOrElse(DNF(DNF.`1`)).invert.andSets
        } yield as1 ∧ as2
      }
      OrSet(ass2)
    }

    /**
      * Return a set of symbols which are used in this expression.
      */
    def symbols: Set[Symbol] = this.trues | this.falses

    /**
      * Return true if this expression is contradiction.
      */
    def isFalseEvery: Boolean = !(this.trues & this.falses).isEmpty

    /**
      * Check to contain given symbol in this expression.
      */
    def contains(s: Symbol) = this.trues.contains(s) || this.falses.contains(s)

    override def toString: String = {
      if (this.isEmpty) {
        "1"
      } else {
        (this.trues | this.falses).toArray
          .sortBy { _.name }
          .map { s =>
            if (this.falses.contains(s)) s"¬${s.name}" else s.name
          }
          .mkString(" ∧ ")
      }
    }
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
    def invert: OrSet = {
      val ass = this.andSets
        .map(_.invert)
        .foldLeft(Set(DNF.`1`)) {
          case (as, OrSet(as2)) =>
            for {
              a <- as
              a2 <- as2
            } yield a ∧ a2
        }
      OrSet(ass)
    }

    /**
      * Replace symbol by given map.
      */
    def replace(map: Symbol => Option[OrSet]): OrSet =
      this.andSets.map(_.replace(map)).foldLeft(DNF.`0`) { _ ∨ _ }

    /**
      * Return a set of symbols which are used in this expression.
      */
    def symbols = this.andSets.foldLeft(Set.empty[Symbol]) { _ | _.symbols }

    /**
      * Convert to full DNF.
      */
    def toFull: OrSet = {
      val andSets1 = this.andSets.filter { !_.isFalseEvery }
      val symbols = andSets.foldLeft(Set.empty[Symbol]) { _ | _.symbols }
      val andSets2 = symbols.foldLeft(andSets1) { (andSets, s) =>
        andSets.flatMap { andSet =>
          if (andSet.contains(s)) {
            Set(andSet)
          } else {
            Set(andSet ∧ DNF.symbol(s), andSet ∧ DNF.not(s))
          }
        }
      }
      OrSet(andSets2)
    }

    override def toString: String =
      if (this.andSets.isEmpty) {
        "0"
      } else {
        this.andSets
          .map { as =>
            if (as.isEmpty || as.isSingleton || this.andSets.size == 1)
              as.toString
            else s"($as)"
          }
          .mkString(" ∨ ")
      }
  }

  val `1` = AndSet(Set.empty, Set.empty)
  val `0` = OrSet(Set.empty)

  implicit class AndSetDSL(val self: AndSet) extends AnyVal {
    def ∧(other: AndSet): AndSet = self.concat(other)
    def ∨(other: AndSet): OrSet = OrSet(Set(self, other))
    def ∨(other: OrSet): OrSet = OrSet(Set(self) | other.andSets)
  }

  implicit class OrSetDSL(val self: OrSet) extends AnyVal {
    def ∨(other: AndSet): OrSet = OrSet(self.andSets | Set(other))
    def ∨(other: OrSet): OrSet = OrSet(self.andSets | other.andSets)
  }
}
