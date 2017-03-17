package com.github.makenowjust.bfa

sealed abstract class BExpr {
  import BExpr._

  def simplify: BExpr = this match {
    case And(left, right) => (left.simplify, right.simplify) match {
      case (True, True)  => True
      case (False, _)    => False
      case (_, False)    => False
      case (left, right) => And(left, right)
    }

    case Or(left, right) => (left.simplify, right.simplify) match {
      case (False, False) => False
      case (True, _)      => True
      case (_, True)      => True
      case (lefy, right)  => Or(left, right)
    }

    case Not(node) => node.simplify match {
      case True => False
      case False => True
      case node => Not(node)
    }

    case True      => True
    case False     => False
    case Var(name) => Var(name)
  }

  def apply(env: Map[Symbol, BExpr]): BExpr = this match {
    case And(left, right) => And(left(env), right(env))
    case Or(left, right)  => Or(left(env), right(env))
    case Not(node)        => Not(node(env))
    case True             => True
    case False            => False
    case Var(name)        => env.getOrElse(name, Var(name))
  }

  def toBoolean: Option[Boolean] = this match {
    case True  => Some(true)
    case False => Some(false)
    case _     => None
  }

  def &(right: BExpr): BExpr = And(this, right)
  def |(right: BExpr): BExpr = Or(this, right)
  def unary_~ = Not(this)
}

object BExpr {
  final case class  And(left: BExpr, right: BExpr) extends BExpr
  final case class  Or(left: BExpr, right: BExpr)  extends BExpr
  final case class  Not(node: BExpr)               extends BExpr
        case object True                           extends BExpr
        case object False                          extends BExpr
  final case class  Var(name: Symbol)              extends BExpr

  def apply(value: Boolean): BExpr = if (value) True else False

  implicit def symbol2bexpr(name: Symbol): BExpr = Var(name)
}
