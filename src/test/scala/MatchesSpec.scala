package bfa

import org.scalatest.{WordSpec, MustMatchers}

class MatchesSpec extends WordSpec with MustMatchers {
  val fixtures =
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
    )

  "AST#matches" must {
    fixtures.foreach {
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
  }

  "BFA#matches" must {
    fixtures.foreach {
      case (s, (oks, fails)) =>
        oks.foreach { ok =>
          s"""match "$s" against "$ok"""" in {
            BFA.from(Parser.parse(s).get).matches(ok) must be(true)
          }
        }

        fails.foreach { fail =>
          s"""not match "$s" against "$fail"""" in {
            BFA.from(Parser.parse(s).get).matches(fail) must not be (true)
          }
        }
    }
  }
}
