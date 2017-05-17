package com.github.makenowjust.bfa

import BExpr._
import RegexAST._

final case class BFA(
  transExpr: Map[(Symbol, Char), BExpr],
  transSet: Map[(Symbol, Char), BExpr],
  initExpr: BExpr,
  initSet: Set[Symbol]) {
  val statesExpr: Set[Symbol] = initExpr.vars ++ (transExpr.keySet map { _._1 }) ++ (transExpr.values flatMap { _.vars })
  val statesSet: Set[Symbol]  = initSet ++ (transSet.keySet map { _._1 }) ++ (transSet.values flatMap { _.vars })

  def apply(text: String): Boolean = {
    def tg(q: Symbol, c: Char) = transExpr.getOrElse((q, c), False).simplify
    def tt(q: Symbol, c: Char)(t: Set[Symbol]) = transSet.getOrElse((q, c), False)(t).toBoolean

    val (g, t) = text.toList.foldLeft((initExpr, initSet)) {
      case ((g, t), c) =>
        val g2 = g((statesExpr map { q => q -> tg(q, c) }).toMap).simplify
        val t2 = statesSet filter { q => tt(q, c)(t) }
        (g2, t2)
    }
    g(t).toBoolean
  }
}

object BFA {
  implicit class Regex2BFA(val re: RegexAST) {
    def toBFA: BFA = {
      val Result(tg, tt, ig, it, ag, at, ht, bt) = from(re)

      val ate = ((at ++ ht) map { q => q -> ag }).toMap
      val tg2 = tg mapValues { g => g(ate) }
      val ig2 = ig(ate)

      BFA(tg2, tt, ig2, it ++ bt)
    }

    private final case class Result(
        transExpr: Map[(Symbol, Char), BExpr],
        transSet: Map[(Symbol, Char), BExpr],
        initExpr: BExpr,
        initSet: Set[Symbol],
        acceptExpr: BExpr,
        acceptSet: Set[Symbol],
        aheadSet: Set[Symbol],
        behindSet: Set[Symbol])

    private[this] def from(e: RegexAST): Result = e match {
      case Alt(l, r)    => fromAlt(l, r)
      case Concat(l, r) => fromConcat(l, r)

      case PositiveLookAhead(e)  => fromPositiveLookAhead(e)
      case NegativeLookAhead(e)  => fromNegativeLookAhead(e)
      case PositiveLookBehind(e) => fromPositiveLookBehind(e)
      case NegativeLookBehind(e) => fromNegativeLookBehind(e)

      case Many(e)     => fromMany(e)
      case Some(e)     => fromSome(e)
      case Optional(e) => fromOptional(e)

      case Literal(c) => fromLiteral(c)
      case Empty      => fromEmpty()
    }

    private[this] def fromAlt(l: RegexAST, r: RegexAST): Result = {
      val Result(tg1, tt1, ig1, it1, ag1, at1, ht1, bt1) = from(l)
      val Result(tg2, tt2, ig2, it2, ag2, at2, ht2, bt2) = from(r)

      Result(
        transExpr  = tg1 ++ tg2,
        transSet   = tt1 ++ tt2,
        initExpr   = ig1 \/ ig2,
        initSet    = it1 ++ it2,
        acceptExpr = ag1 \/ ag2,
        acceptSet  = at1 ++ at2,
        aheadSet   = ht1 ++ ht2,
        behindSet  = bt1 ++ bt2)
    }

    private[this] def fromConcat(l: RegexAST, r: RegexAST): Result = {
      val Result(tg1, tt1, ig1, it1, ag1, at1, ht1, bt1) = from(l)
      val Result(tg2, tt2, ig2, it2, ag2, at2, ht2, bt2) = from(r)

      val at1e = (at1 map { q => q -> ig2 }).toMap
      val tg12 = tg1 mapValues { g => g(at1e) }
      val ig12 = ig1(at1e)

      val it2e = (it2 map { q => q -> ag1 }).toMap
      val tt22 = tt2 mapValues { g => g(it2e) }
      val ag22 = ag2(it2e)

      Result(
        transExpr  = tg12 ++ tg2,
        transSet   = tt1 ++ tt22,
        initExpr   = ig12,
        initSet    = it1,
        acceptExpr = ag22,
        acceptSet  = at2,
        aheadSet   = ht1 ++ ht2,
        behindSet  = bt1 ++ bt2)
    }

    private[this] def fromPositiveLookAhead(e: RegexAST): Result = {
      val qg = newSymbol()
      val qt = newSymbol()
      val Result(tg, tt, ig, it, ag, at, ht, bt) = from(e)

      Result(
        transExpr  = tg,
        transSet   = tt,
        initExpr   = qg /\ ig,
        initSet    = Set(qt),
        acceptExpr = qt,
        acceptSet  = Set(qg),
        aheadSet   = ht ++ at,
        behindSet  = bt)
    }

    private[this] def fromNegativeLookAhead(e: RegexAST): Result = {
      val Result(tg, tt, ig, it, ag, at, ht, bt) = from(e)

      ???
    }

    private[this] def fromPositiveLookBehind(e: RegexAST): Result = {
      val qg = newSymbol()
      val qt = newSymbol()
      // TODO: `ig` and `at` is unused.
      val Result(tg, tt, ig, it, ag, at, ht, bt) = from(e)

      Result(
        transExpr  = tg,
        transSet   = tt,
        initExpr   = qg,
        initSet    = Set(qt),
        acceptExpr = qt /\ ag,
        acceptSet  = Set(qg),
        aheadSet   = ht,
        behindSet  = bt ++ it)
    }

    private[this] def fromNegativeLookBehind(e: RegexAST): Result = {
      val Result(tg, tt, ig, it, ag, at, ht, bt) = from(e)

      ???
    }

    private[this] def fromMany(e: RegexAST): Result = {
      val qg = newSymbol()
      val qt = newSymbol()
      val Result(tg, tt, ig, it, ag, at, ht, bt) = from(e)

      val ate = (at map { q => q -> ig \/ q }).toMap
      val tg2 = tg mapValues { g => g(ate) }
      val ig2 = ig(ate)

      val ite = (it map { q => q -> ag \/ q }).toMap
      val tt2 = tt mapValues { g => g(ite) }

      Result(
        transExpr  = tg2,
        transSet   = tt2,
        initExpr   = ig2 \/ qg,
        initSet    = it + qt,
        acceptExpr = ag \/ qt,
        acceptSet  = at + qg,
        aheadSet   = ht,
        behindSet  = bt)
    }

    private[this] def fromSome(e: RegexAST): Result = {
      val Result(tg, tt, ig, it, ag, at, ht, bt) = from(e)

      val ate = (at map { q => q -> ig \/ q }).toMap
      val tg2 = tg mapValues { g => g(ate) }
      val ig2 = ig(ate)

      val ite = (it map { q => q -> ag \/ q }).toMap
      val tt2 = tt mapValues { g => g(ite) }

      Result(
        transExpr  = tg2,
        transSet   = tt2,
        initExpr   = ig2,
        initSet    = it,
        acceptExpr = ag,
        acceptSet  = at,
        aheadSet   = ht,
        behindSet  = bt)
    }

    private[this] def fromOptional(e: RegexAST): Result = {
      val qg = newSymbol()
      val qt = newSymbol()
      val Result(tg, tt, ig, it, ag, at, ht, bt) = from(e)

      Result(
        transExpr  = tg,
        transSet   = tt,
        initExpr   = ig \/ qg,
        initSet    = it + qt,
        acceptExpr = ag \/ qt,
        acceptSet  = at + qg,
        aheadSet   = ht,
        behindSet  = bt)
    }

    private[this] def fromLiteral(c: Char): Result = {
      val qg0 = newSymbol()
      val qg1 = newSymbol()
      val qt0 = newSymbol()
      val qt1 = newSymbol()

      Result(
        transExpr  = Map((qg0, c) -> qg1),
        transSet   = Map((qt1, c) -> qt0),
        initExpr   = qg0,
        initSet    = Set(qt0),
        acceptExpr = qt1,
        acceptSet  = Set(qg1),
        aheadSet   = Set.empty,
        behindSet  = Set.empty)
    }

    private[this] def fromEmpty(): Result = {
      val qg = newSymbol()
      val qt = newSymbol()

      Result(
        transExpr  = Map.empty,
        transSet   = Map.empty,
        initExpr   = qg,
        initSet    = Set(qt),
        acceptExpr = qt,
        acceptSet  = Set(qg),
        aheadSet   = Set.empty,
        behindSet  = Set.empty)
    }

    private[this] var count = 0

    private[this] def newSymbol(): Symbol = {
      count += 1
      Symbol(s"q$count")
    }
  }
}
