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

  "replace" must {
    "replace nothing" in {
      DNF(`1`).replace(Map()) must be(DNF(`1`))
      DNF(`0`).replace(Map()) must be(DNF(`0`))
    }

    "replace simple" in {
      DNF(A).replace(Map('A -> B)) must be(DNF(B))
      DNF(`¬A`).replace(Map('A -> B)) must be(DNF(`¬B`))
      DNF(A ∧ B).replace(Map('A -> B, 'B -> B)) must be(DNF(B))
      DNF(A ∧ `¬B`).replace(Map('A -> B, 'B -> B)) must be(DNF(B ∧ `¬B`))
    }

    "replace complex" in {
      DNF(A ∨ `¬B`).replace(Map('A -> B, 'B -> A ∧ B)) must be(
        DNF(B ∨ `¬A` ∨ `¬B`))
      DNF(A ∧ `¬B`).replace(Map('A -> B, 'B -> A ∧ B)) must be(
        DNF((B ∧ `¬A`) ∨ (B ∧ `¬B`)))
    }
  }

  "symbols" must {
    "return a set of symbols" in {
      DNF(A).symbols must be(Set('A))
      DNF(`¬A`).symbols must be(Set('A))
      DNF(A ∧ B).symbols must be(Set('A, 'B))
      DNF(A ∨ B).symbols must be(Set('A, 'B))
    }

    "return an empty set against true or false" in {
      DNF(`0`).symbols must be(Set.empty)
      DNF(`1`).symbols must be(Set.empty)
    }
  }

  "isFalseEvery" must {
    "return true if the expression is contraction" in {
      (A ∧ `¬A`).isFalseEvery must be(true)
    }

    "return false if not" in {
      A.isFalseEvery must be(false)
      B.isFalseEvery must be(false)
      (A ∧ B).isFalseEvery must be(false)
    }
  }

  "contains" must {
    "return true if it has this symbol" in {
      A.contains('A) must be(true)
      `¬A`.contains('A) must be(true)
      (A ∧ `¬B`).contains('A) must be(true)
      (A ∧ `¬B`).contains('B) must be(true)
    }

    "return false if it doesn't have this symbol" in {
      A.contains('B) must be(false)
      `¬A`.contains('B) must be(false)
      (A ∧ `¬B`).contains('C) must be(false)
      (A ∧ `¬B`).contains('C) must be(false)
    }
  }

  "toFull" must {
    "do nothing if this is already full DNF" in {
      DNF(A).toFull must be(DNF(A))
      DNF(`¬A`).toFull must be(DNF(`¬A`))
      DNF(A ∧ B).toFull must be(DNF(A ∧ B))
      DNF((A ∧ `¬B`) ∨ (`¬A` ∧ B)).toFull must be(DNF((A ∧ `¬B`) ∨ (`¬A` ∧ B)))
    }

    "convert to full DNF" in {
      DNF(A ∨ B).toFull must be(DNF((A ∧ B) ∨ (A ∧ `¬B`) ∨ (`¬A` ∧ B)))
    }
  }

  "toString" must {
    "return a string without parenthesis if empty set" in {
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
      DNF(`¬A` ∧ B).toString must be("¬A ∧ B")
    }

    "return complex string" in {
      DNF((A ∧ `¬B`) ∨ (C ∧ `¬D`)).toString must be("(A ∧ ¬B) ∨ (C ∧ ¬D)")
    }
  }
}
