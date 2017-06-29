package bfa

import org.scalatest._

class DNFSpec extends WordSpec with MustMatchers {
  import DNF._

  "evaluate" must {
    val empty = Set.empty[Symbol]
    val a = Set('a)
    val b = Set('b)
    val ab = Set('a, 'b)
    val bc = Set('b, 'c)

    "evaluate set of empty set as true" in {
      DNF(Set((empty, empty))).evaluate(Set.empty) must be(true)
    }

    "evaluate set of subset of given set as true" in {
      DNF(Set((a, empty))).evaluate(a) must be(true)
      DNF(Set((a, empty))).evaluate(ab) must be(true)
      DNF(Set((a, empty), (b, empty))).evaluate(bc) must be(true)

      DNF(Set((empty, a))).evaluate(Set.empty) must be(true)
      DNF(Set((empty, a))).evaluate(b) must be(true)
    }

    "evaluate empty set as false" in {
      DNF(Set.empty).evaluate(Set()) must be(false)
    }

    "evaluate set of no subset of given set as false" in {
      DNF(Set((a, empty))).evaluate(b) must be(false)
      DNF(Set((ab, empty))).evaluate(bc) must be(false)

      DNF(Set((empty, a))).evaluate(a) must be(false)
      DNF(Set((empty, ab))).evaluate(a) must be(false)
    }
  }

  "invert" must {
    val empty = Set.empty[Symbol]
    val a = Set('a)
    val b = Set('b)
    val ab = Set('a, 'b)
    val bc = Set('b, 'c)

    "invert empty set" in {
      DNF(Set.empty).invert must be(DNF(Set((empty, empty))))
    }

    "invert set of empty set" in {
      DNF(Set((empty, empty))).invert must be(DNF(Set.empty))
    }

    "invert set" in {
      DNF(Set((a, empty))).invert must be(DNF(Set((empty, a))))
      DNF(Set((empty, a))).invert must be(DNF(Set((a, empty))))
      DNF(Set((ab, empty))).invert must be(DNF(Set((empty, a), (empty, b))))
      DNF(Set((empty, ab))).invert must be(DNF(Set((a, empty), (b, empty))))
      DNF(Set((a, b))).invert must be(DNF(Set((empty, a), (b, empty))))
      DNF(Set((a, empty), (b, empty))).invert must be(DNF(Set((empty, ab))))
      DNF(Set((empty, a), (empty, b))).invert must be(DNF(Set((ab, empty))))
    }
  }

  "toString" must {
    "return a string with parenthesis" in {
      DNF(Set((Set('A, 'B), Set.empty))).toString must be("(A ∧ B)")
      DNF(Set((Set('A), Set('B)))).toString must be("(A ∧ ¬B)")
      DNF(Set((Set('A), Set('B)), (Set('C), Set('D)))).toString must be(
        "(A ∧ ¬B) ∨ (C ∧ ¬D)")
    }

    "return a string without parenthesis if singleton or empty set" in {
      DNF(Set((Set.empty, Set.empty))).toString must be("1")
      DNF(Set((Set('A), Set.empty))).toString must be("A")
      DNF(Set((Set.empty, Set('A)))).toString must be("¬A")
      DNF(Set.empty).toString must be("0")
    }
  }
}
