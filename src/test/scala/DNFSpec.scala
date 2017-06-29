package bfa

import org.scalatest._

class DNFSpec extends WordSpec with MustMatchers {
  import DNF._

  val A = DNF.symbol('A)
  val B = DNF.symbol('B)
  val C = DNF.symbol('C)
  val D = DNF.symbol('D)
  val `¬A` = DNF.not('A)
  val `¬B` = DNF.not('B)
  val `¬C` = DNF.not('C)
  val `¬D` = DNF.not('D)

  "evaluate" must {
    "evaluate true as true" in {
      DNF(`1`).evaluate(Set.empty) must be(true)
    }

    "evaluate set of subset of given set as true" in {
      DNF(A).evaluate(Set('A)) must be(true)
      DNF(A).evaluate(Set('A, 'B)) must be(true)
      DNF(A ∨ B).evaluate(Set('B, 'C)) must be(true)

      DNF(`¬A`).evaluate(Set.empty) must be(true)
      DNF(`¬A`).evaluate(Set('B)) must be(true)
    }

    "evaluate empty set as false" in {
      DNF(A).evaluate(Set.empty) must be(false)
    }

    "evaluate set of no subset of given set as false" in {
      DNF(A).evaluate(Set('B)) must be(false)
      DNF(A ∧ B).evaluate(Set('B, 'C)) must be(false)

      DNF(`¬A`).evaluate(Set('A)) must be(false)
      DNF(`¬A` ∧ `¬B`).evaluate(Set('A)) must be(false)
    }
  }

  "invert" must {
    "invert false to true" in {
      DNF(`0`).invert must be(DNF(`1`))
    }

    "invert true to false" in {
      DNF(`1`).invert must be(DNF(`0`))
    }

    "invert set" in {
      DNF(A).invert must be(DNF(`¬A`))
      DNF(`¬A`).invert must be(DNF(A))
      DNF(A ∧ B).invert must be(DNF(`¬A` ∨ `¬B`))
      DNF(`¬A` ∧ `¬B`).invert must be(DNF(A ∨ B))
      DNF(A ∧ `¬B`).invert must be(DNF(`¬A` ∨ B))
      DNF(A ∨ B).invert must be(DNF(`¬A` ∧ `¬B`))
      DNF(`¬A` ∨ `¬B`).invert must be(DNF(A ∧ B))
    }
  }

  "toString" must {
    "return a string without patenthesis if empty set" in {
      DNF(`1`).toString must be("1")
      DNF(`0`).toString must be("0")
    }

    "return a string without parenthesis if singleton" in {
      DNF(A).toString must be("A")
      DNF(`¬A`).toString must be("¬A")
    }

    "return a string without parenthesis if single expression" in {
      DNF(A ∧ B).toString must be("A ∧ B")
      DNF(A ∧ `¬B`).toString must be("A ∧ ¬B")
    }

    "return complex string" in {
      DNF((A ∧ `¬B`) ∨ (C ∧ `¬D`)).toString must be("(A ∧ ¬B) ∨ (C ∧ ¬D)")
    }
  }
}
