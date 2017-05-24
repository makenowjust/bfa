package bfa

import org.scalatest._

class ASTSpec extends WordSpec with MustMatchers {
  import AST._

  "reverse" must {
    List(
      ("foo", Concat(Literal('o'), Concat(Literal('o'), Literal('f')))),
      ("fo|o", Alt(Concat(Literal('o'), Literal('f')), Literal('o')))
    ).foreach { case (s, n) =>
      s"""reverse "$s"""" in {
        Parser.parse(s).get.reverse must be(n)
      }
    }

    List(
      Alt(Literal('a'), Literal('b')),
      PositiveLookAhead(Empty),
      NegativeLookAhead(Empty),
      PositiveLookBehind(Empty),
      NegativeLookBehind(Empty),
      PositiveLookAhead(Concat(Literal('f'), Literal('o'))),
      NegativeLookAhead(Concat(Literal('f'), Literal('o'))),
      PositiveLookBehind(Concat(Literal('f'), Literal('o'))),
      NegativeLookBehind(Concat(Literal('f'), Literal('o'))),
      Star(Empty),
      Plus(Empty),
      Quest(Empty),
      Literal('a'),
      Empty
    ).foreach { n =>
      s"""not change $n""" in {
        n.reverse must be(n)
      }
    }
  }

  "matches" must {
    List(
      ("foo", (List("foo"), List("bar"))),
      ("foo?", (List("foo", "fo"), List("fooo", "f"))),
      ("foo+", (List("foo", "foooo", "fooooooo"), List("fo"))),
      ("f?oo*", (List("foo", "o", "oo", "ooo", "foooo"), List("f", "fof"))),
      ("fo(o|f)(?<=(o|f)o)", (List("foo"), List("fof", "ffo"))),
      ("fo(o|f)(?<!(a|r)r)", (List("foo", "fof"), List("far", "frr"))),
      ("(?=f(o|f))(f|o)oo", (List("foo"), List("ooo", "ffo", "ofo"))),
      ("(?!b(b|a))(f|o)oo", (List("foo", "ooo"), List("bao", "bbo")))
    ).foreach { case (s, (oks, fails)) =>
      oks.foreach { ok =>
        s"""match "$s" against "$ok"""" in {
          Parser.parse(s).get.matches(ok) must be(true)
        }
      }

      fails.foreach { fail =>
        s"""not match "$s" against "$fail"""" in {
          Parser.parse(s).get.matches(fail) must not be(true)
        }
      }
    }
  }
}
