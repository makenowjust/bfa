package bfa

import org.scalatest.{WordSpec, MustMatchers}

class ASTSpec extends WordSpec with MustMatchers {
  import AST._

  "reverse" must {
    List(
      ("foo", Concat(Literal('o'), Concat(Literal('o'), Literal('f')))),
      ("fo|o", Alt(Concat(Literal('o'), Literal('f')), Literal('o')))
    ).foreach {
      case (s, n) =>
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
      ("(foo|)*",
       (List("", "foo", "foofoo", "foofoofoo"), List("f", "fo", "foof"))),
      ("(foo)+",
       (List("foo", "foofoo", "foofoofoo"), List("", "f", "fo", "foof"))),
      ("f?oo*", (List("foo", "o", "oo", "ooo", "foooo"), List("f", "fof"))),
      ("fo*o", (List("fo", "foo", "fooo"), List("f", "ff", "fof"))),
      ("fo+o", (List("foo", "fooo"), List("f", "fo", "ff", "fof"))),
      ("fo(o|f)(?<=(o|f)*(o|f)o)", (List("foo"), List("fof", "ffo"))),
      ("fo(o|f)(?<!(a|r)*(a|r)r)", (List("foo", "fof"), List("far", "frr"))),
      ("(?=f(o|f)(o|f)*)(f|o)oo", (List("foo"), List("ooo", "ffo", "ofo"))),
      ("(?!b(b|a)(b|a)*)(f|o)oo", (List("foo", "ooo"), List("bao", "bbo"))),
      ("(?=f(o|f)o(?<=(o|f)*o(o|f))(o|f)*)(o|f)(o|f)(o|f)",
       (List("foo"), List("ff", "ofo", "ffo", "fff", "fooo"))),
      ("(o|f)(o|f)(o|f)(?<=(?=(o|f)o(o|f)*)(o|f)*f(o|f)o)",
       (List("foo"), List("ff", "ofo", "ffo", "fff", "fooo")))
    ).foreach {
      case (s, (oks, fails)) =>
        oks.foreach { ok =>
          s"""match "$s" against "$ok"""" in {
            Parser.parse(s).get.matches(ok) must be(true)
          }
        }

        fails.foreach { fail =>
          s"""not match "$s" against "$fail"""" in {
            Parser.parse(s).get.matches(fail) must not be (true)
          }
        }
    }

    "match against large string without StackOverflow" in {
      Parser.parse("a*").get.matches("a" * 10000) must be(true)
    }
  }
}
