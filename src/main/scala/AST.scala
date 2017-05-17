package bfa

sealed abstract class AST

object AST {
  final case class Alt(left: AST, right: AST)    extends AST
  final case class Concat(left: AST, right: AST) extends AST

  final case class PositiveLookAhead(node: AST)  extends AST
  final case class NegativeLookAhead(node: AST)  extends AST
  final case class PositiveLookBehind(node: AST) extends AST
  final case class NegativeLookBehind(node: AST) extends AST

  final case class Star(node: AST)  extends AST
  final case class Plus(node: AST)  extends AST
  final case class Quest(node: AST) extends AST

  final case class  Literal(char: Char) extends AST
        case object Empty               extends AST
}
