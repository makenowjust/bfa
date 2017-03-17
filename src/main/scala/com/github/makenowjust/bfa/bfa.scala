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

    def θ(w: Map[Symbol, Boolean]): Symbol = {
      val n = (0 /: Q) { (n, q) => (n << 1) | (if (w(q)) 1 else 0) }
      Symbol(s"q$n")
    }

    val `2^Q` = (Set(Set.empty[Symbol]) /: Q) { (set, q) =>
      set.union(set.map(_ + q))
    }
    val Q_DFA = `2^Q`.map { S => θ(W(V(S))) }
    val Σ_DFA = Σ
    val δ_DFA = `2^Q`.flatMap { S =>
      Σ.map { a =>
        val v = V(S)
        (θ(W(v)), a) -> θ(W(Q.map(q =>
            q -> g.getOrElse((q, a), BExpr.False)(v).simplify).toMap))
      }
    }.toMap
    val q0_DFA = θ(W(f))
    val F_DFA = `2^Q`.filter { S => h(V(S)).simplify.toBoolean.get }
      .map { S => θ(W(V(S))) }

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

  def fromRegex(re: RegexAST, Σ: Set[Char]): BFA = {
    var id = 0
    val Q = mutable.Set.empty[Symbol]
    def q(): Symbol = {
      val q = Symbol(s"q$id")
      id += 1
      Q.add(q)
      q
    }
    val g = mutable.Map.empty[(Symbol, Char), BExpr]
    def cons(re: RegexAST, prev: BExpr, next: Set[Symbol]): BExpr =
      re match {
        case Alt(nodes) =>
          ((False: BExpr) /: nodes) { (e, re) =>
            Or(e, cons(re, prev, next))
          }
        case Concat(nodes) => {
          val prev1 = (prev /: nodes.dropRight(1)) { (prev, re) =>
            cons(re, prev, Set())
          }
          cons(nodes.last, prev1, next)
        }

        case Many(node) => {
          val now = q()
          cons(node, prev | now, next + now) | prev | now
        }
        case Some(node) => {
          val now = q()
          cons(node, prev | now, next + now)
        }
        case Optional(node) => {
          cons(node, prev, next) | prev
        }

        case Literal(char) => {
          val now = q()
          (next + now).foreach { q => g((q, char)) = prev.simplify }
          ((False: BExpr) /: (next + now)) { Or(_, _) }
        }
        case Any => {
          val now = q()
          (next + now).foreach { q =>
            Σ.foreach { c =>
              g((q, c)) = prev.simplify
            }
          }
          ((False: BExpr) /: (next + now)) { Or(_, _) }
        }
        case Empty => prev
      }

    val q0 = q()
    val F = Set(q0)
    val h = cons(re, q0, Set()).simplify

    BFA(Set(Q.toSeq: _*), Σ, Map(g.toSeq: _*), h, F)
  }
}
