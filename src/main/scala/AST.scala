package bfa

import scala.annotation.tailrec

sealed abstract class AST {
  import AST._

  def matches(s: String): Boolean = matches(Reader(s), re => re.eof )

  private def matches(re: Reader, cont: (Reader) => Boolean): Boolean = {
    def always(re: Reader) = true

    def loop(n: AST, re: Reader, cont: (Reader) => Boolean): Boolean =
      n.matches(re, re => loop(n, re, cont)) || cont(re)

    this match {
      case Alt(l, r)    => l.matches(re, cont) || r.matches(re, cont)
      case Concat(l, r) => l.matches(re, re => r.matches(re, cont))

      case PositiveLookAhead(n)  =>
        n.matches(re.forward, always _) && cont(re)
      case NegativeLookAhead(n)  =>
        !n.matches(re.forward, always _) && cont(re)
      case PositiveLookBehind(n) =>
        n.reverse.matches(re.backward, always _) && cont(re)
      case NegativeLookBehind(n) =>
        !n.reverse.matches(re.backward, always _) && cont(re)

      case Star(n)  => loop(n, re, cont)
      case Plus(n)  => n.matches(re, re => loop(n, re, cont))
      case Quest(n) => n.matches(re, cont) || cont(re)

      case Literal(c) =>
        if (re.current == Some(c)) cont(re.next) else false
      case Empty      => cont(re)
    }
  }

  def reverse: AST = this match {
    case Alt(l, r)    => Alt(l.reverse, r.reverse)
    case Concat(l, r) => Concat(r.reverse, l.reverse)

    case PositiveLookAhead(n)  => PositiveLookAhead(n)
    case NegativeLookAhead(n)  => NegativeLookAhead(n)
    case PositiveLookBehind(n) => PositiveLookBehind(n)
    case NegativeLookBehind(n) => NegativeLookBehind(n)

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
