package bfa

import scala.util.parsing.combinator._

class Parser extends RegexParsers {
  import RegexAST._

  def alt: Parser[RegexAST] =
    concat * ("|" ^^^ { Alt(_, _) })

  def concat: Parser[RegexAST] =
    condition.* ^^ {
      case Nil => Empty
      case (node :: Nil) => node
      case (node :: nodes) => nodes.foldLeft(node) { Concat(_, _) }
    }

  def condition: Parser[RegexAST] =
    "(?="  ~> alt <~ ")" ^^ { PositiveLookAhead(_) }  |
    "(?!"  ~> alt <~ ")" ^^ { NegativeLookAhead(_) }  |
    "(?<=" ~> alt <~ ")" ^^ { PositiveLookBehind(_) } |
    "(?<!" ~> alt <~ ")" ^^ { NegativeLookBehind(_) } |
    repeat

  def repeat: Parser[RegexAST] =
    for {
      node <- atom;
      repeated <- {
        "*" ^^^ { Star(node) }     |
        "+" ^^^ { Plus(node) }     |
        "?" ^^^ { Quest(node) }
      }.?
    } yield repeated.getOrElse(node)

  def atom: Parser[RegexAST] =
    "(" ~> alt <~ ")"                                           |
    """[^()*+?|]""".r ^^ { (s: String) => Literal(s.charAt(0)) }
}

object Parser extends Parser {
  def parse(input: String): Option[RegexAST] = parseAll(alt, input) match {
    case Success(re, _) => Some(re)
    case _              => None
  }
}
