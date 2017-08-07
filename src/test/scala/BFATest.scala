package bfa

import org.scalatest.{WordSpec, MustMatchers}

class BFATest extends WordSpec with MustMatchers {
  import DNF._

  val A = DNF.symbol('A)
  val B = DNF.symbol('B)
  val C = DNF.symbol('C)
  val D = DNF.symbol('D)
  val `¬A` = DNF.not('A)
  val `¬B` = DNF.not('B)
  val `¬C` = DNF.not('C)
  val `¬D` = DNF.not('D)

  "matches" must {
    "match an empty string against an empty BFA" in {
      val bfa = BFA(DNF(A), Map.empty, Set('A), List.empty)
      bfa.matches("") must be(true)
    }

    "match a string against a simple BFA" in {
      val bfa = BFA(DNF(A), Map(('A, 'a') -> DNF(B)), Set('B), List.empty)
      bfa.matches("a") must be(true)
    }
  }

  "from" must {
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
      ("(?=f(o|f)(o|f)*)(f|o)oo", (List("foo"), List("ooo", "ffo", "ofo"))),
      ("(?=f(o|f)o(?<=(o|f)*o(o|f))(o|f)*)(o|f)(o|f)(o|f)",
       (List("foo"), List("ff", "ofo", "ffo", "fff", "fooo"))),
      ("(o|f)(o|f)(o|f)(?<=(?=(o|f)o(o|f)*)(o|f)*f(o|f)o)",
       (List("foo"), List("ff", "ofo", "ffo", "fff", "fooo")))
    ).foreach {
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
