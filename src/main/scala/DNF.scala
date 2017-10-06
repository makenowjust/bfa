package bfa

object DNF {
  private type Or[V] = Set[And[V]]
  private type And[V] = (Set[V], Set[V])

  final case class Full[V] private[bfa] (dnf: DNF[V])

  trait From[-A] {
    type Var

    def from(a: A): DNF[Var]
  }

  def from[A](a: A)(implicit F: From[A]): DNF[F.Var] = F.from(a)

  trait FromBooleanImplicit[V] {
    implicit object FromBoolean extends From[Boolean] {
      type Var = V

      def from(a: Boolean): DNF[V] = a match {
        case true  => DNF(Set((Set.empty, Set.empty)))
        case false => DNF(Set.empty)
      }
    }
  }
}

/**
  * DNF is Disjunctive Normal Form.
  */
final case class DNF[V] private (private[bfa] val or: DNF.Or[V]) {
  import DNF._

  def ∧(other: DNF[V]): DNF[V] = DNF {
    for {
      (ts1, fs1) <- or
      (ts2, fs2) <- other.or
    } yield (ts1 | ts2, fs1 | fs2)
  }

  def ∨(other: DNF[V]): DNF[V] = DNF(or | other.or)

  def evaluate(vs: Set[V]): Boolean = or.exists {
    case (ts, fs) =>
      (ts subsetOf vs) && (fs & vs).isEmpty
  }

  def invert(implicit fromBoolean: FromBooleanImplicit[V]): DNF[V] = {
    import fromBoolean._

    or.foldLeft(DNF.from(true)) {
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

  def replace[W](f: V => DNF[W])(
      implicit fromBoolean: FromBooleanImplicit[W]): DNF[W] = {
    import fromBoolean._

    def r(dnf: DNF[W], vs: Set[V], f: V => DNF[W]): DNF[W] =
      vs.foldLeft(dnf) { case (dnf, v) => dnf ∧ f(v) }

    or.foldLeft(DNF.from(false)) {
      case (dnf, (ts, fs)) =>
        dnf ∨ r(r(DNF.from(true), ts, f), fs, f(_).invert)
    }
  }

  def toFull: Full[V] =
    Full(DNF {
      val or1 = or.filter { case (ts, fs)  => !ts.isEmpty || !fs.isEmpty }
      val vs = or1.flatMap { case (ts, fs) => ts | fs }
      vs.foldLeft(or1) {
        case (or, v) =>
          or.flatMap {
            case (ts, fs) =>
              if (ts.contains(v) || fs.contains(v))
                Set((ts, fs))
              else
                Set((ts + v, fs), (ts, fs + v))
          }
      }
    })

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
