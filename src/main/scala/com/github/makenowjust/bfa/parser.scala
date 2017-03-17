package com.github.makenowjust.bfa

import scala.util.parsing.combinator._

class RegexParser extends RegexParsers {
  import RegexAST._

  def alt: Parser[RegexAST] =
    concat * ("|" ^^^ { Alt(_, _) })

  def concat: Parser[RegexAST] =
    condition.? >> {
      _.map(left => concat ^^ {
        case Empty => left
        case right => Concat(left, right)
      }).getOrElse(success(Empty))
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
        "*" ^^^ { Many(node) }     |
        "+" ^^^ { Some(node) }     |
        "?" ^^^ { Optional(node) }
      }.?
    } yield repeated.getOrElse(node)

  def atom: Parser[RegexAST] =
    "(" ~> alt <~ ")"                                             |
    "." ^^^ { Any }                                               |
    """[^.()*+?|]""".r ^^ { (s: String) => Literal(s.charAt(0)) }

  def eof: Parser[String] = """\z""".r
}

object RegexParser extends RegexParser {
  def parse(input: String): Option[RegexAST] = parseAll(alt, input) match {
    case Success(re, _) => Some(re)
    case _              => None
  }
}
