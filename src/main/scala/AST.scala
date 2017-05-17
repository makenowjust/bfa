package bfa

sealed abstract class AST {
  import AST._

  def reverse: AST = this match {
    case Alt(l, r)    => Alt(l.reverse, r.reverse)
    case Concat(l, r) => Concat(r.reverse, l.reverse)

    case PositiveLookAhead(n)  => PositiveLookAhead(n.reverse)
    case NegativeLookAhead(n)  => NegativeLookAhead(n.reverse)
    case PositiveLookBehind(n) => PositiveLookBehind(n.reverse)
    case NegativeLookBehind(n) => NegativeLookBehind(n.reverse)

    case Star(n)  => Star(n.reverse)
    case Plus(n)  => Plus(n.reverse)
    case Quest(n) => Quest(n.reverse)

    case Literal(c) => Literal(c)
    case Empty      => Empty
  }
}

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
