package bfa

import scala.util.parsing.combinator._

class Parser extends RegexParsers {
  import AST._

  def alt: Parser[AST] =
    concat * ("|" ^^^ { Alt(_, _) })

  def concat: Parser[AST] =
    condition.* ^^ {
      case Nil        => Empty
      case (n :: Nil) => n
      case (n :: ns)  => ns.foldLeft(n) { Concat(_, _) }
    }

  def condition: Parser[AST] =
    "(?=" ~> alt <~ ")" ^^ { PositiveLookAhead(_) } |
      "(?!" ~> alt <~ ")" ^^ { NegativeLookAhead(_) } |
      "(?<=" ~> alt <~ ")" ^^ { PositiveLookBehind(_) } |
      "(?<!" ~> alt <~ ")" ^^ { NegativeLookBehind(_) } |
      repeat

  def repeat: Parser[AST] =
    for {
      n1 <- atom;
      n2 <- {
        "*" ^^^ { Star(n1) } |
          "+" ^^^ { Plus(n1) } |
          "?" ^^^ { Quest(n1) }
      }.?
    } yield n2.getOrElse(n1)

  def atom: Parser[AST] =
    "(" ~> alt <~ ")" |
    "[" ~> """[^\]]*""".r <~ "]" ^^ { s => s.foldLeft(Fail: AST) { (l, c) => l | Literal(c) } } |
      """[^()\[\]*+?|]""".r ^^ { (s: String) =>
        Literal(s.charAt(0))
      }
}

object Parser extends Parser {
  def parse(input: String): Option[AST] = parseAll(alt, input) match {
    case Success(re, _) => Some(re)
    case _              => None
  }
}
