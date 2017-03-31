package com.github.makenowjust.bfa

sealed abstract class BExpr {
  import BExpr._

  def simplify: BExpr = this match {
    case And(l, r) => simplifyAnd(l.simplify, r.simplify)
    case Or(l, r)  => simplifyOr(l.simplify, r.simplify)
    case Not(e)    => simplifyNot(e.simplify)
    case True      => True
    case False     => False
    case Var(q)    => Var(q)
  }

  private[this] def simplifyAnd(l: BExpr, r: BExpr) = (l, r) match {
    case (True, r)  => r
    case (l, True)  => l
    case (False, _) => False
    case (_, False) => False
    case (l, r)     => And(l, r)
  }

  private[this] def simplifyOr(l: BExpr, r: BExpr) = (l, r) match {
    case (False, e) => e
    case (e, False) => e
    case (True, _)  => True
    case (_, True)  => True
    case (lefy, r)  => Or(l, r)
  }

  private[this] def simplifyNot(e: BExpr) = e match {
    case True  => False
    case False => True
    case _     => Not(e)
  }

  def vars: Set[Symbol] = this match {
    case And(l, r) => l.vars ++ r.vars
    case Or(l, r)  => l.vars ++ r.vars
    case Not(e)    => e.vars
    case True      => Set.empty
    case False     => Set.empty
    case Var(q)    => Set(q)
  }

  def apply(env: Map[Symbol, BExpr]): BExpr = this match {
    case And(left, right) => And(left(env), right(env))
    case Or(left, right)  => Or(left(env), right(env))
    case Not(node)        => Not(node(env))
    case True             => True
    case False            => False
    case Var(name)        => env.getOrElse(name, Var(name))
  }

  def apply(set: Set[Symbol]): BExpr = this(set map { s => s -> True } toMap)

  override def toString(): String = this match {
    case And(l: Or, r: Or)  => s"($l) /\\ ($r)"
    case And(l: Or, r)      => s"($l) /\\ $r"
    case And(l, r: Or)      => s"$l /\\ ($r)"
    case And(l, r)          => s"$l /\\ $r"
    case Or(l: And, r: And) => s"($l) \\/ ($r)"
    case Or(l: And, r)      => s"($l) \\/ $r"
    case Or(l, r: And)      => s"$l \\/ ($r)"
    case Or(l, r)           => s"$l \\/ $r"
    case Not(e: And)        => s"¬($e)"
    case Not(e: Or)         => s"¬($e)"
    case Not(e)             => s"¬$e"
    case True               => "1"
    case False              => "0"
    case Var(s)             => s.name
  }

  def /\(right: BExpr): BExpr = And(this, right)
  def \/(right: BExpr): BExpr = Or(this, right)

  def toBoolean: Boolean = this.simplify match {
    case True => true
    case _ => false
  }
}

object BExpr {
  import scala.language.implicitConversions

  final case class  And(left: BExpr, right: BExpr) extends BExpr
  final case class  Or(left: BExpr, right: BExpr)  extends BExpr
  final case class  Not(expr: BExpr)               extends BExpr
        case object True                           extends BExpr
        case object False                          extends BExpr
  final case class  Var(name: Symbol)              extends BExpr

  def /\[T <% BExpr](syms: Set[T]): BExpr = syms.foldRight(True: BExpr) { _ /\ _  }.simplify
  def \/[T <% BExpr](syms: Set[T]): BExpr = syms.foldRight(False: BExpr) { _ \/ _  }.simplify
  def ¬(expr: BExpr): BExpr = Not(expr)

  implicit def boolean2bexpr(value: Boolean): BExpr = if (value) True else False
  implicit def symbol2bexpr(name: Symbol): BExpr = Var(name)
}
