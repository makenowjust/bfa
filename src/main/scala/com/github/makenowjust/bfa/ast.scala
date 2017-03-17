package com.github.makenowjust.bfa

sealed abstract class RegexAST

object RegexAST {
  final case class Alt(nodes: List[RegexAST])    extends RegexAST
  final case class Concat(nodes: List[RegexAST]) extends RegexAST

  final case class PositiveLookAhead(node: RegexAST)  extends RegexAST
  final case class NegativeLookAhead(node: RegexAST)  extends RegexAST
  final case class PositiveLookBehind(node: RegexAST) extends RegexAST
  final case class NegativeLookBehind(node: RegexAST) extends RegexAST

  final case class Many(node: RegexAST)     extends RegexAST
  final case class Some(node: RegexAST)     extends RegexAST
  final case class Optional(node: RegexAST) extends RegexAST

  final case class  Literal(char: Char) extends RegexAST
        case object Any                 extends RegexAST
        case object Empty               extends RegexAST
}
