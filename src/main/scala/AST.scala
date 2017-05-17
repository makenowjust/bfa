package bfa

import scala.annotation.tailrec

sealed abstract class AST {
  import AST._

  def matches(s: String): Boolean = matches(Reader(s)) exists { _.eof }

  private def matches(re: Reader): Option[Reader] = this match {
    case Alt(l, r)    => matchesAlt(re, l, r)
    case Concat(l, r) => matchesConcat(re, l, r)

    case PositiveLookAhead(n)  => matchesPositiveLookAhead(re, n)
    case NegativeLookAhead(n)  => matchesNegativeLookAhead(re, n)
    case PositiveLookBehind(n) => matchesPositiveLookBehind(re, n)
    case NegativeLookBehind(n) => matchesNegativeLookBehind(re, n)

    case Star(n)  => matchesStar(re, n)
    case Plus(n)  => matchesPlus(re, n)
    case Quest(n) => matchesQuest(re, n)

    case Literal(c) => matchesLiteral(re, c)
    case Empty      => matchesEmpty(re)
  }

  private[this] def matchesAlt(re: Reader, l: AST, r: AST) =
    l.matches(re) orElse r.matches(re)

  private[this] def matchesConcat(re: Reader, l: AST, r: AST) =
    l.matches(re) flatMap { r.matches(_) }

  private[this] def matchesPositiveLookAhead(re: Reader, n: AST) =
    n.matches(re.forward) map { _ => re }

  private[this] def matchesNegativeLookAhead(re: Reader, n: AST) =
    n.matches(re.forward) orElse Some(re)

  private[this] def matchesPositiveLookBehind(re: Reader, n: AST) =
    n.reverse.matches(re.backward) map { _ => re }

  private[this] def matchesNegativeLookBehind(re: Reader, n: AST) =
    n.reverse.matches(re.backward) orElse Some(re)

  @tailrec
  private[this] def matchesStar(re: Reader, n: AST): Option[Reader] =
    n.matches(re) match {
      case Some(re) => matchesStar(re, n)
      case None     => Some(re)
    }

  private[this] def matchesPlus(re: Reader, n: AST) =
    n.matches(re) flatMap { matchesStar(_, n) }

  private[this] def matchesQuest(re: Reader, n: AST) =
    n.matches(re) orElse Some(re)

  private[this] def matchesLiteral(re: Reader, c: Char) =
    if (re.current == Some(c)) Some(re.next) else None

  private[this] def matchesEmpty(re: Reader) = Some(re)

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
