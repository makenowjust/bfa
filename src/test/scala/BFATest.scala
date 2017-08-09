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
}
