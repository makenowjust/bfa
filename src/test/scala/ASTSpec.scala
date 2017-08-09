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
    "match against large string without StackOverflow" in {
      Parser.parse("a*").get.matches("a" * 10000) must be(true)
    }
  }
}
