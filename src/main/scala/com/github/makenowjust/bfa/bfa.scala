package com.github.makenowjust.bfa

import BExpr._
import RegexAST._

final case class BFA(
  states: Set[Symbol],
  trans: Map[(Symbol, Char), BExpr],
  init: BExpr,
  finish: Set[Symbol]
) {
  def apply(text: String): BExpr = {
    val last = text.toList.foldLeft(init) { (f, c) =>
      f(states map { s => s -> trans.getOrElse((s, c), False) } toMap)
    }
    last(states map { s => s -> (finish contains s: BExpr) } toMap).simplify
  }
}

object BFA {
  implicit final class RegexAST2BFA(r: RegexAST) {

    def toBFA: BFA = {
      val (ss, tr, i, f, l) = from(r)
      BFA(ss, tr, i.simplify, f union l)
    }

    private type ToResult = (
      Set[Symbol],
      Map[(Symbol, Char), BExpr],
      BExpr,
      Set[Symbol],
      Set[Symbol])

    private def from(r: RegexAST): ToResult = r match {
      case Alt(l, r)    => fromAlt(l, r)
      case Concat(l, r) => fromConcat(l, r)

      case Many(n)     => fromMany(n)
      case Some(n)     => fromSome(n)
      case Optional(n) => fromOptional(n)

      case PositiveLookAhead(n)  => fromPositiveLookAhead(n)
      case NegativeLookAhead(n)  => fromNegativeLookAhead(n)

      case Literal(c) => fromLiteral(c)
      case Empty      => fromEmpty()
    }

    private def fromAlt(l: RegexAST, r: RegexAST): ToResult = {
      val (ss1, tr1, i1, f1, l1) = from(l)
      val (ss2, tr2, i2, f2, l2) = from(r)
      (ss1 union ss2, tr1 ++ tr2, i1 \/ i2, f1 union f2, l1 union l2)
    }

    private def fromConcat(l: RegexAST, r: RegexAST): ToResult = {
      val (ss1, tr1, i1, f1, l1) = from(l)
      val (ss2, tr2, i2, f2, l2) = from(r)
      val env = f1 map { s => s -> (s \/ i2).simplify } toMap
      val tr3 = (tr1 mapValues { e => e(env) }) ++ tr2
      (ss1 union ss2, tr3, i1(env), f2, l1 union l2)
    }

    private def fromMany(n: RegexAST): ToResult = {
      val s0 = newSymbol()
      val (ss, tr, i, f, l) = from(n)
      val env = f map { s => s -> (s \/ i).simplify } toMap
      val tr1 = tr mapValues { e => e(env) }
      (ss + s0, tr1, i \/ s0, f + s0, l)
    }

    private def fromSome(n: RegexAST): ToResult = {
      val (ss, tr, i, f, l) = from(n)
      val env = f map { s => s -> (s \/ i).simplify } toMap
      val tr1 = tr mapValues { e => e(env) }
      (ss, tr1, i, f, l)
    }

    private def fromOptional(n: RegexAST): ToResult = {
      val s0 = newSymbol()
      val (ss, tr, i, f, l) = from(n)
      (ss + s0, tr, s0 \/ i, f + s0, l)
    }

    private def fromPositiveLookAhead(n: RegexAST): ToResult = {
      val s = newSymbol()
      val (ss, tr, i, f, l) = from(n)
      (ss + s, tr, i /\ s, Set(s), f union l)
    }

    private def fromNegativeLookAhead(n: RegexAST): ToResult = {
      val s = newSymbol()
      val (ss, tr, i, f, l) = from(n)
      (ss + s, tr, ¬(i) /\ s, Set(s), f union l)
    }

    private def fromLiteral(c: Char): ToResult = {
      val s = newSymbol()
      val t = newSymbol()
      (Set(s, t), Map((s, c) -> t), s, Set(t), Set.empty)
    }

    private def fromEmpty(): ToResult = {
      val s = newSymbol()
      (Set(s), Map.empty, s, Set(s), Set.empty)
    }

    var count = 0

    def newSymbol(): Symbol = {
      count += 1
      Symbol(s"s$count")
    }
  }
}
