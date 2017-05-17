package bfa

import scala.util.parsing.combinator._

class Parser extends RegexParsers {
  import AST._

  def alt: Parser[AST] =
    concat * ("|" ^^^ { Alt(_, _) })

  def concat: Parser[AST] =
    condition.* ^^ {
      case Nil => Empty
      case (node :: Nil) => node
      case (node :: nodes) => nodes.foldLeft(node) { Concat(_, _) }
    }

  def condition: Parser[AST] =
    "(?="  ~> alt <~ ")" ^^ { PositiveLookAhead(_) }  |
    "(?!"  ~> alt <~ ")" ^^ { NegativeLookAhead(_) }  |
    "(?<=" ~> alt <~ ")" ^^ { PositiveLookBehind(_) } |
    "(?<!" ~> alt <~ ")" ^^ { NegativeLookBehind(_) } |
    repeat

  def repeat: Parser[AST] =
    for {
      node <- atom;
      repeated <- {
        "*" ^^^ { Star(node) }     |
        "+" ^^^ { Plus(node) }     |
        "?" ^^^ { Quest(node) }
      }.?
    } yield repeated.getOrElse(node)

  def atom: Parser[AST] =
    "(" ~> alt <~ ")"                                           |
    """[^()*+?|]""".r ^^ { (s: String) => Literal(s.charAt(0)) }
}

object Parser extends Parser {
  def parse(input: String): Option[AST] = parseAll(alt, input) match {
    case Success(re, _) => Some(re)
    case _              => None
  }
}
