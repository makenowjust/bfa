package bfa

sealed abstract class RegexAST

object RegexAST {
  final case class Alt(left: RegexAST, right: RegexAST)    extends RegexAST
  final case class Concat(left: RegexAST, right: RegexAST) extends RegexAST

  final case class PositiveLookAhead(node: RegexAST)  extends RegexAST
  final case class NegativeLookAhead(node: RegexAST)  extends RegexAST
  final case class PositiveLookBehind(node: RegexAST) extends RegexAST
  final case class NegativeLookBehind(node: RegexAST) extends RegexAST

  final case class Star(node: RegexAST)     extends RegexAST
  final case class Plus(node: RegexAST)     extends RegexAST
  final case class Quest(node: RegexAST) extends RegexAST

  final case class  Literal(char: Char) extends RegexAST
        case object Empty               extends RegexAST
}
