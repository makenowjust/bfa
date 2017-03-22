package com.github.makenowjust.bfa

sealed abstract class BExpr {
  import BExpr._

  def simplify: BExpr = this match {
    case And(left, right) => simplifyAnd(left.simplify, right.simplify)
    case Or(left, right) => simplifyOr(left.simplify, right.simplify)
    case Not(node) => simplifyNot(node.simplify)
    case True      => True
    case False     => False
    case Var(name) => Var(name)
  }

  private[this] def simplifyAnd(left: BExpr, right: BExpr) = (left, right) match {
    case (True, node)  => node
    case (node, True)  => node
    case (False, _)    => False
    case (_, False)    => False
    case (left, right) => And(left, right)
  }

  private[this] def simplifyOr(left: BExpr, right: BExpr) = (left, right) match {
    case (False, node)  => node
    case (node, False)  => node
    case (True, _)      => True
    case (_, True)      => True
    case (lefy, right)  => Or(left, right)
  }

  private[this] def simplifyNot(node: BExpr) = node match {
    case True => False
    case False => True
    case _ => Not(node)
  }

  def apply(env: Map[Symbol, BExpr]): BExpr = this match {
    case And(left, right) => And(left(env), right(env))
    case Or(left, right)  => Or(left(env), right(env))
    case Not(node)        => Not(node(env))
    case True             => True
    case False            => False
    case Var(name)        => env.getOrElse(name, Var(name))
  }

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
}

object BExpr {
  final case class  And(left: BExpr, right: BExpr) extends BExpr
  final case class  Or(left: BExpr, right: BExpr)  extends BExpr
  final case class  Not(expr: BExpr)               extends BExpr
        case object True                           extends BExpr
        case object False                          extends BExpr
  final case class  Var(name: Symbol)              extends BExpr

  def \/[T <% BExpr](syms: Set[T]): BExpr = syms.foldRight(False: BExpr) { _ \/ _  }
    .simplify
  def ¬(expr: BExpr): BExpr = Not(expr)

  implicit def boolean2bexpr(value: Boolean): BExpr =
    if (value) True else False
  implicit def symbol2bexpr(name: Symbol): BExpr = Var(name)
}
