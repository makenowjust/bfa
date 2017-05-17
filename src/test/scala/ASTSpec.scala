package bfa

import org.scalatest._

class ASTSpec extends WordSpec with MustMatchers {
  import AST._

  "AST.reverse" should {
    List[(String, AST)](
      ("foo", Concat(Literal('o'), Concat(Literal('o'), Literal('f')))),
      ("fo|o", Alt(Concat(Literal('o'), Literal('f')), Literal('o'))),
      ("(?=fo)", PositiveLookAhead(Concat(Literal('o'), Literal('f')))),
      ("(?!fo)", NegativeLookAhead(Concat(Literal('o'), Literal('f')))),
      ("(?<=fo)", PositiveLookBehind(Concat(Literal('o'), Literal('f')))),
      ("(?<!fo)", NegativeLookBehind(Concat(Literal('o'), Literal('f'))))
    ).foreach { case (s, n) =>
      s"""reverses "$s"""" in {
        Parser.parse(s).get.reverse must be(n)
      }
    }

    List[AST](
      Alt(Literal('a'), Literal('b')),
      PositiveLookAhead(Empty),
      NegativeLookAhead(Empty),
      PositiveLookBehind(Empty),
      NegativeLookBehind(Empty),
      Star(Empty),
      Plus(Empty),
      Quest(Empty),
      Literal('a'),
      Empty
    ).foreach { n =>
      s"""not changes $n""" in {
        n.reverse must be(n)
      }
    }
  }
}
