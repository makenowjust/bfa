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
    def tg(q: Symbol, c: Char) = transExpr.getOrElse((q, c), False) simplify
    def tt(q: Symbol, c: Char)(t: Set[Symbol]) = transSet.getOrElse((q, c), False)(t) toBoolean

    val (g, t) = text.toList.foldLeft((initExpr, initSet)) {
      case ((g, t), c) =>
        val g2 = g(statesExpr map { q => q -> tg(q, c) } toMap) simplify
        val t2 = statesSet filter { q => tt(q, c)(t) }
        (g2, t2)
    }
    g(t) toBoolean
  }
}

object BFA {
  implicit class Regex2BFA(val re: RegexAST) {
    def toBFA: BFA = {
      val Result(tg, tt, ig, it, ag, at) = from(re)

      val ate = at map { q => q -> ag } toMap
      val tg2 = tg mapValues { g => g(ate) }
      val ig2 = ig(ate)

      BFA(tg2, tt, ig2, it)
    }

    private[this] final case class Result(
      transExpr: Map[(Symbol, Char), BExpr],
      transSet: Map[(Symbol, Char), BExpr],
      initExpr: BExpr,
      initSet: Set[Symbol],
      acceptExpr: BExpr,
      acceptSet: Set[Symbol])

    private[this] def from(e: RegexAST): Result = e match {
      case Alt(l, r)    => fromAlt(l, r)
      case Concat(l, r) => fromConcat(l, r)

      case PositiveLookAhead(e)  => fromPositiveLookAhead(e)
      case NegativeLookAhead(e)  => fromNegativeLookAhead(e)
      case PositiveLookBehind(e) => fromPositiveLookBehind(e)
      case NegativeLookBehind(e) => fromNegativeLookBehind(e)

      //case Many(e)     => fromMany(e)
      //case Some(e)     => fromSome(e)
      //case Optional(e) => fromOptional(e)

      case Literal(c) => fromLiteral(c)
      case Empty      => fromEmpty()
    }

    private[this] def fromAlt(l: RegexAST, r: RegexAST): Result = {
      val Result(tg1, tt1, ig1, it1, ag1, at1) = from(l)
      val Result(tg2, tt2, ig2, it2, ag2, at2) = from(r)

      Result(
        transExpr  = tg1 ++ tg2,
        transSet   = tt1 ++ tt2,
        initExpr   = ig1 \/ ig2,
        initSet    = it1 ++ it2,
        acceptExpr = ag1 \/ ag2,
        acceptSet  = at1 ++ at2)
    }

    private[this] def fromConcat(l: RegexAST, r: RegexAST): Result = {
      val Result(tg1, tt1, ig1, it1, ag1, at1) = from(l)
      val Result(tg2, tt2, ig2, it2, ag2, at2) = from(r)

      val at1e = at1 map { q => q -> ig2 } toMap
      val tg12 = tg1 mapValues { g => g(at1e) }
      val ig12 = ig1(at1e)
      val ag12 = ag1(at1e)

      val it2e = it2 map { q => q -> ag12 } toMap
      val tt22 = tt2 mapValues { g => g(it2e) }
      val ag22 = ag2(it2e)

      Result(
        transExpr  = tg12 ++ tg2,
        transSet   = tt1 ++ tt22,
        initExpr   = ig12,
        initSet    = it1,
        acceptExpr = ag22,
        acceptSet  = at2)
    }

    private[this] def fromPositiveLookAhead(e: RegexAST): Result = {
      val Result(tg, tt, ig, it, ag, at) = from(e)

      ???
    }

    private[this] def fromNegativeLookAhead(e: RegexAST): Result = {
      val Result(tg, tt, ig, it, ag, at) = from(e)

      ???
    }

    private[this] def fromPositiveLookBehind(e: RegexAST): Result = {
      val Result(tg, tt, ig, it, ag, at) = from(e)

      ???
    }

    private[this] def fromNegativeLookBehind(e: RegexAST): Result = {
      val Result(tg, tt, ig, it, ag, at) = from(e)

      ???
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
        acceptSet  = Set(qg1))
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
        acceptSet  = Set(qg))
    }

    private[this] var count = 0

    private[this] def newSymbol(): Symbol = {
      count += 1
      Symbol(s"q$count")
    }
  }
}
