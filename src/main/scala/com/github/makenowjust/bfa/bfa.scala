package com.github.makenowjust.bfa

import scala.collection.mutable

final case class BFA(
  Q: Set[Symbol],
  Σ: Set[Char],
  g: Map[(Symbol, Char), BExpr],
  h: BExpr,
  F: Set[Symbol]
) {
  private def V(S: Set[Symbol]) =
    Q.map(q => q -> BExpr(S.contains(q))).toMap

  lazy val f: Map[Symbol, BExpr] = V(F)

  def apply(w: String): Boolean =
    h((f /: w){ (v, a) => Q.map(q =>
      q -> g.getOrElse((q, a), BExpr.False)(v).simplify).toMap
    }).simplify.toBoolean.get

  def toDFA: DFA = {
    def W = (_: Map[Symbol, BExpr]).mapValues(_.toBoolean.get)

    def q_DFA(w: Map[Symbol, Boolean]): Symbol = {
      val n = (0 /: Q) { (n, q) => (n << 1) | (if (w(q)) 1 else 0) }
      Symbol(s"q$n")
    }

    val `2^Q` = (Set(Set.empty[Symbol]) /: Q) { (set, q) =>
      set.union(set.map(_ + q))
    }
    val Q_DFA = `2^Q`.map { S => q_DFA(W(V(S))) }
    val Σ_DFA = Σ
    val δ_DFA = `2^Q`.flatMap { S =>
      Σ.map { a =>
        val v = V(S)
        (q_DFA(W(v)), a) -> q_DFA(W(Q.map(q =>
            q -> g.getOrElse((q, a), BExpr.False)(v).simplify).toMap))
      }
    }.toMap
    val q0_DFA = q_DFA(W(f))
    val F_DFA = `2^Q`.filter { S => h(V(S)).simplify.toBoolean.get }
      .map { S => q_DFA(W(V(S))) }

    DFA(Q_DFA, Σ_DFA, δ_DFA, q0_DFA, F_DFA)
  }
}

object BFA {
  import BExpr._
  import RegexAST._

  val example1 = BFA(
    Set('q1, 'q2),
    Set('a', 'b'),
    Map(
      ('q1, 'a') -> ( 'q1 | ~'q2),
      ('q2, 'a') -> (~'q1 & ~'q2),
      ('q1, 'b') -> ( 'q1 & ~'q2),
      ('q2, 'b') -> (~'q1 |  'q2)
    ),
    ~'q1 | 'q2,
    Set('q2)
  )

  private class Regex2BFA(re: RegexAST, Σ: Set[Char]) {
    var id = 0
    val Q = mutable.Set.empty[Symbol]
    def q(): Symbol = {
      val q = Symbol(s"q$id")
      id += 1
      Q.add(q)
      q
    }

    val g = mutable.Map.empty[(Symbol, Char), BExpr]
    val q0 = q()
    val F = Set(q0)
    val h = cons(re, q0, Set()).simplify

    private[BFA] def bfa = BFA(Set(Q.toSeq: _*), Σ, Map(g.toSeq: _*), h, F)

    def cons(re: RegexAST, prev: BExpr, next: Set[Symbol]): BExpr =
      re match {
        case Alt(nodes)    => consAlt(nodes, prev, next)
        case Concat(nodes) => consConcat(nodes, prev, next)

        case Many(node)     => consMany(node, prev, next)
        case Some(node)     => consSome(node, prev, next)
        case Optional(node) => consOptional(node, prev, next)

        case Literal(char) => consLiteral(char, prev, next)
        case Any           => consAny(prev, next)
        case Empty         => consEmpty(prev, next)
      }

    def consAlt(nodes: List[RegexAST], prev: BExpr, next: Set[Symbol]): BExpr =
      ((False: BExpr) /: nodes) { (e, re) =>
        Or(e, cons(re, prev, next))
      }
    def consConcat(nodes: List[RegexAST], prev: BExpr, next: Set[Symbol]): BExpr = {
      val prev1 = (prev /: nodes.dropRight(1)) { (prev, re) =>
        cons(re, prev, Set())
      }
      cons(nodes.last, prev1, next)
    }

    def consMany(node: RegexAST, prev: BExpr, next: Set[Symbol]): BExpr = {
      val now = q()
      cons(node, prev | now, next + now) | prev | now
    }
    def consSome(node: RegexAST, prev: BExpr, next: Set[Symbol]): BExpr = {
      val now = q()
      cons(node, prev | now, next + now)
    }
    def consOptional(node: RegexAST, prev: BExpr, next: Set[Symbol]): BExpr =
      cons(node, prev, next) | prev

    def consLiteral(char: Char, prev: BExpr, next: Set[Symbol]): BExpr = {
      val now = q()
      (next + now).foreach { q => g((q, char)) = prev.simplify }
      ((False: BExpr) /: (next + now)) { Or(_, _) }
    }
    def consAny(prev: BExpr, next: Set[Symbol]): BExpr = {
      val now = q()
      (next + now).foreach { q =>
        Σ.foreach { c =>
          g((q, c)) = prev.simplify
        }
      }
      ((False: BExpr) /: (next + now)) { Or(_, _) }
    }
    def consEmpty(prev: BExpr, next: Set[Symbol]): BExpr = prev
  }

  def fromRegex(re: RegexAST, Σ: Set[Char]): BFA = new Regex2BFA(re, Σ).bfa
}
