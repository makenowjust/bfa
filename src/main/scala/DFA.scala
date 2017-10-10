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

  def minimize: DFA = {
    val vs = symbols
    val cs = trans.iterator.map { case ((_, c), _) => c }.toSet

    val dead = Symbol("d")

    def delta2(q: Symbol, p: Symbol, c: Char): (Symbol, Symbol) = {
      val q1 = trans.getOrElse((q, c), dead)
      val p1 = trans.getOrElse((p, c), dead)
      (q1, p1)
    }

    val d0 = for {
      q <- last
      p <- (vs + dead) diff last
    } yield (q, p)

    val `di+1`: Set[(Symbol, Symbol)] => Set[(Symbol, Symbol)] = {
      di => di | (for {
        q <- symbols
        p <- symbols
        if cs.exists { c => di.contains(delta2(q, p, c)) }
      } yield (q, p))
    }

    lazy val di: Stream[Set[(Symbol, Symbol)]] = d0 #:: di.map(`di+1`)
    val d = (di zip di.tail).dropWhile { case ((di, di1)) => di != di1 }.head._1

    def indi(q: Symbol): Set[Symbol] = vs.filter { p => !d.contains((q, p)) }

    var id = 0
    val qMap = new mutable.HashMap[Set[Symbol], Symbol]
    def indi2q(indi: Set[Symbol]): Symbol = qMap.getOrElseUpdate(indi, { id += 1; Symbol(s"v$id")})

    val i = indi2q(indi(init))
    val t = vs.foldLeft((Set.empty[Symbol], Map.empty[(Symbol, Char), Symbol])) {
      case ((done, t), v) =>
        val q = indi2q(indi(v))
        if (done.contains(q)) (done, t)
        else (done + q, t ++ cs.iterator.map { c => trans.get((v, c)).map { p => (q, c) -> indi2q(indi(p)) }.toList }.flatten.toMap)
    }._2
    val l = last.map { q => indi2q(indi(q)) }

    DFA(i, t, l)
  }
}

object DFA {
  def from(mbfa: MBFA): DFA = mbfa.toDFA
}
