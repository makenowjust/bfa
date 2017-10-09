package bfa

import scala.util.control.TailCalls._

object AST {
  final case class Alt(left: AST, right: AST) extends AST {
    def altFlatten: List[AST] = this match {
      case Alt(l: Alt, r: Alt) => l.altFlatten ++ r.altFlatten
      case Alt(l: Alt, r)      => l.altFlatten ++ List(r)
      case Alt(l, r: Alt)      => List(l) ++ r.altFlatten
      case Alt(l, r)           => List(l, r)
    }

    def isCharClass: Boolean = altFlatten.forall{ case _: Literal => true; case _ => false }
  }

  final case class Concat(left: AST, right: AST) extends AST {
    def concatFlatten: List[AST] = this match {
      case Concat(l: Concat, r: Concat) => l.concatFlatten ++ r.concatFlatten
      case Concat(l: Concat, r)      => l.concatFlatten ++ List(r)
      case Concat(l, r: Concat)      => List(l) ++ r.concatFlatten
      case Concat(l, r)           => List(l, r)
    }
  }

  final case class PositiveLookAhead(node: AST) extends AST
  final case class NegativeLookAhead(node: AST) extends AST
  final case class PositiveLookBehind(node: AST) extends AST
  final case class NegativeLookBehind(node: AST) extends AST

  final case class Star(node: AST) extends AST
  final case class Plus(node: AST) extends AST
  final case class Quest(node: AST) extends AST

  final case class Literal(char: Char) extends AST
  case object Empty extends AST
  case object Fail extends AST
}

sealed abstract class AST {
  import AST._

  def matches(s: String): Boolean =
    matches(Reader(s), re => done(re.eof)).result

  private def matches(re: Reader,
                      cont: (Reader) => TailRec[Boolean]): TailRec[Boolean] = {
    val success = (re: Reader) => done(re.eof)
    def and(right: => TailRec[Boolean]) =
      (left: Boolean) => if (left) tailcall(right) else done(false)
    def or(right: => TailRec[Boolean]) =
      (left: Boolean) => if (left) done(true) else tailcall(right)

    def loop(n: AST,
             re: Reader,
             cont: (Reader) => TailRec[Boolean]): TailRec[Boolean] =
      tailcall(
        n.matches(re, re2 => if (re == re2) cont(re) else loop(n, re2, cont)))
        .flatMap(or(cont(re)))

    this match {
      case Alt(l, r) =>
        tailcall(l.matches(re, cont)).flatMap(or(r.matches(re, cont)))
      case Concat(l, r) =>
        tailcall(l.matches(re, re => tailcall(r.matches(re, cont))))

      case PositiveLookAhead(n) =>
        tailcall(n.matches(re.forward, success)).flatMap(and(cont(re)))
      case NegativeLookAhead(n) =>
        tailcall(n.matches(re.forward, success)).map(!_).flatMap(and(cont(re)))
      case PositiveLookBehind(n) =>
        tailcall(n.reverse.matches(re.backward, success))
          .flatMap(and(cont(re)))
      case NegativeLookBehind(n) =>
        tailcall(n.reverse.matches(re.backward, success))
          .map(!_)
          .flatMap(and(cont(re)))

      case Star(n)  => loop(n, re, cont)
      case Plus(n)  => tailcall(n.matches(re, re => loop(n, re, cont)))
      case Quest(n) => tailcall(n.matches(re, cont)).flatMap(or(cont(re)))

      case Literal(c) =>
        if (re.current == Some(c)) tailcall(cont(re.next)) else done(false)
      case Empty => tailcall(cont(re))
      case Fail  => done(false)
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
    case Fail       => Fail
  }

  def |(that: AST): AST = (this, that) match {
    case (Fail, Fail)     => Fail
    case (l, Fail)        => l
    case (Fail, r)        => r
    case (l, Empty)       => l.?
    case (Empty, r)       => r.?
    case (l, r) if l == r => l
    case (l, r)           => Alt(l, r)
  }

  def * : AST = this match {
    case Fail  => Empty
    case Empty => Empty
    case Star(n) => Star(n)
    case Quest(n) => Star(n)
    case _     => Star(this)
  }

  def ? : AST = this match {
    case Fail  => Empty
    case Empty => Empty
    case Star(n) => Star(n)
    case Quest(n) => Quest(n)
    case _     => Quest(this)
  }

  def ~(that: AST): AST = (this, that) match {
    case (_, Fail)                    => Fail
    case (Fail, _)                    => Fail
    case (l, Empty)                   => l
    case (Empty, r)                   => r
    case (Star(l), Star(r)) if l == r => Star(l)
    case (Plus(l), r) if l == r       => Plus(l)
    case (l, Plus(r)) if l == r       => Plus(l)
    case (Star(l), r) if l == r       => Plus(l)
    case (l, Star(r)) if l == r       => Plus(l)
    case (l, r)                       => Concat(l, r)
  }

  override def toString: String = this match {
    case n: Alt if n.isCharClass => s"[${n.altFlatten.map { case Literal(c) => c; case _ => "" }.mkString}]"
    case n: Alt => n.altFlatten.mkString("(", "|", ")")
    case n: Concat => n.concatFlatten.mkString
    case PositiveLookAhead(n) => s"(?=$n)"
    case NegativeLookAhead(n) => s"(?!$n)"
    case PositiveLookBehind(n) => s"(?<=$n)"
    case NegativeLookBehind(n) => s"(?<!$n)"
    case Star(n) if n.needParen => s"($n)*"
    case Star(n) => s"$n*"
    case Plus(n) if n.needParen => s"($n)+"
    case Plus(n) => s"$n+"
    case Quest(n) if n.needParen => s"($n)?"
    case Quest(n) => s"$n?"
    case Literal(c) => c.toString
    case Empty => "()"
    case Fail => "[]"
  }

  def needParen: Boolean = this match {
    case n: Alt => !n.isCharClass
    case n: PositiveLookAhead => true
    case n: NegativeLookAhead => true
    case n: PositiveLookBehind => true
    case n: NegativeLookBehind => true
    case n: Concat => true
    case n: Star => true
    case n: Plus => true
    case n: Quest => true
    case n: Literal => false
    case Empty => false
    case Fail => false
  }
}
