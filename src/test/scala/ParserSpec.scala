package bfa

import org.scalatest._

class ParserSpec extends WordSpec with MustMatchers {
  import AST._

  "parse" must {
    List[(String, AST)](
      ("", Empty),
      ("f", Literal('f')),
      ("fo", Concat(Literal('f'), Literal('o'))),
      ("foo", Concat(Concat(Literal('f'), Literal('o')), Literal('o'))),
      ("f|o", Alt(Literal('f'), Literal('o'))),
      ("f|o|o", Alt(Alt(Literal('f'), Literal('o')), Literal('o'))),
      ("f|o|o|", Alt(Alt(Alt(Literal('f'), Literal('o')), Literal('o')), Empty)),
      ("f*", Star(Literal('f'))),
      ("f+", Plus(Literal('f'))),
      ("f?", Quest(Literal('f'))),
      ("a(?=b)", Concat(Literal('a'), PositiveLookAhead(Literal('b')))),
      ("a(?!b)", Concat(Literal('a'), NegativeLookAhead(Literal('b')))),
      ("(?<=a)b", Concat(PositiveLookBehind(Literal('a')), Literal('b'))),
      ("(?<!a)b", Concat(NegativeLookBehind(Literal('a')), Literal('b')))
    ).foreach { case (s, t) =>
      s"""parse "$s"""" in {
        Parser.parse(s) must be(Some(t))
      }
    }

    List(
      "*", "+", "?",
      "(", ")", ")("
    ).foreach { s =>
      s"""not parse "$s"""" in {
        Parser.parse(s) must be(None)
      }
    }
  }
}
