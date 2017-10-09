package bfa

object DNF {
  private type Or[V] = Set[And[V]]
  private type And[V] = (Set[V], Set[V])

  def symbol[V](v: V): DNF[V] = DNF(Set((Set(v), Set.empty)))

  def one[V]: DNF[V] = DNF(Set((Set.empty, Set.empty)))
  def zero[V]: DNF[V] = DNF(Set.empty)
}

/**
  * DNF is Disjunctive Normal Form.
  */
final case class DNF[V] private (private[bfa] val or: DNF.Or[V]) {
  import DNF._

  def isOne: Boolean = this == DNF.one
  def isZero: Boolean = this == DNF.zero

  def ∧(that: DNF[V]): DNF[V] =
    if (this == that) this
    else if (this.isOne) that
    else if (that.isOne) this
    else if (this.isZero || that.isZero) DNF.zero
    else DNF {
      for {
        (ts1, fs1) <- this.or
        (ts2, fs2) <- that.or
      } yield (ts1 | ts2, fs1 | fs2)
    }

  def ∨(that: DNF[V]): DNF[V] =
    if (this == that) this
    else if (this.isZero) that
    else if (that.isZero) this
    else if (this.isOne || that.isOne) DNF.one
    else DNF(or | that.or)

  def symbols: Set[V] = or.flatMap { case (ts, fs) => ts | fs }

  def evaluate(vs: Set[V]): Boolean = or.exists {
    case (ts, fs) =>
      (ts subsetOf vs) && (fs & vs).isEmpty
  }

  def invert: DNF[V] = {
    or.foldLeft(DNF.one[V]) {
      case (dnf, (ts, fs)) =>
        val or21 = ts.map { v =>
          ((Set.empty, Set(v))): And[V]
        }.toSet
        val or22 = fs.map { v =>
          ((Set(v), Set.empty)): And[V]
        }.toSet
        dnf ∧ DNF(or21 | or22)
    }
  }

  def replace[W](f: V => DNF[W]): DNF[W] = {
    def r(dnf: DNF[W], vs: Set[V], f: V => DNF[W]): DNF[W] =
      vs.foldLeft(dnf) { case (dnf, v) => dnf ∧ f(v) }

    or.foldLeft(DNF.zero[W]) {
      case (dnf, (ts, fs)) =>
        dnf ∨ r(r(DNF.one, ts, f), fs, f(_).invert)
    }
  }

  override def toString: String = or.toList match {
    case List() => "0"
    case or =>
      or.map {
          case (ts, fs) =>
            (ts.map(_.toString).toList ++ fs.map(v => s"¬$v").toList) match {
              case List()  => "1"
              case List(v) => v
              case vs      => vs.toArray.sorted.mkString("(", " ∧ ", ")")
            }
        }
        .toArray
        .sorted
        .mkString(" ∨ ")
  }
}
