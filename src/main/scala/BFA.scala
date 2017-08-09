package bfa

final case class BFA(
    init: DNF.OrSet,
    trans: Map[(Symbol, Char), DNF.OrSet],
    last: Set[Symbol],
    transOr: List[(Set[Symbol], Set[Symbol])]
) {

  def matches(s: String): Boolean =
    s.foldLeft(init) { (e, c) =>
        val e2 = e.replace { s =>
          trans.get((s, c))
        }
        transOr.foldLeft(e2) {
          case (e, (ss, r)) =>
            if (e.evaluate(ss))
              e.replace { s =>
                Some(DNF(if (r.contains(s)) DNF.`1` else DNF.symbol(s)))
              } else e
        }
      }
      .evaluate(last)
}

object BFA {
  import AST._

  def from(node: AST): BFA =
    new Converter().convert(node)

  private final class Converter {

    type Result = (DNF.OrSet,
                   Map[(Symbol, Char), DNF.OrSet],
                   Set[Symbol],
                   DNF.OrSet,
                   Set[Symbol],
                   List[(Set[Symbol], Set[Symbol])])

    def convert(node: AST): BFA = {
      val (init, trans, last, initOr, lastSet, transOr) =
        this.convertInternal(node)
      BFA(init ∨ initOr, trans, last ++ lastSet, transOr)
    }

    var symbolId = 0

    def newSymbol(): Symbol = {
      symbolId += 1
      Symbol(s"a$symbolId")
    }

    def convertInternal(node: AST): Result = node match {
      case Alt(left, right) => {
        val (i1, t1, l1, io1, ls1, to1) = this.convertInternal(left)
        val (i2, t2, l2, io2, ls2, to2) = this.convertInternal(right)
        (i1 ∨ i2, t1 ++ t2, l1 ++ l2, io1 ∨ io2, ls1 ++ ls2, to1 ++ to2)
      }

      case Concat(left, right) => {
        val (i1, t1, l1, io1, ls1, to1) = this.convertInternal(left)
        val (i2, t2, l2, io2, ls2, to2) = this.convertInternal(right)
        val i3 = i1.replace { s =>
          Some(if (l1.contains(s)) i2 else DNF(DNF.symbol(s)))
        }
        val t3 = t1.map {
          case (k, v) =>
            k -> v.replace { s =>
              Some(if (l1.contains(s)) i2 else DNF(DNF.symbol(s)))
            }
        }.toMap ++ t2
        (i3, t3, l2, io1 ∨ io2, ls1 ++ ls2, to1 ++ to2)
      }

      case PositiveLookAhead(node) => {
        val (i, t, l, io, ls, to) = this.convertInternal(node)
        val s = this.newSymbol()
        val i2 = i ∧ DNF.symbol(s)
        (i2, t, Set(s), io, ls ++ l, to)
      }

      case NegativeLookAhead(node) => {
        val (i, t, l, io, ls, to) = this.convertInternal(node)
        val s = this.newSymbol()
        val i2 = i.invert ∧ DNF.symbol(s)
        (i2, t, Set(s), io, ls ++ l, to)
      }

      case PositiveLookBehind(node) => {
        val (i, t, l, io, ls, to) = this.convertInternal(node)
        val s1 = this.newSymbol()
        val s2 = this.newSymbol()
        (DNF(DNF.symbol(s1) ∧ DNF.symbol(s2)),
         t,
         Set(s1),
         io ∨ i,
         ls,
         to ++ List((l ++ ls, Set(s2))))
      }

      case NegativeLookBehind(node) => {
        val (i, t, l, io, ls, to) = this.convertInternal(node)
        val s1 = this.newSymbol()
        val s2 = this.newSymbol()
        (DNF(DNF.symbol(s1) ∧ DNF.not(s2)),
         t,
         Set(s1),
         io ∨ i,
         ls,
         to ++ List((l ++ ls, Set(s2))))
      }

      case Star(node) => {
        val s = this.newSymbol()
        val (i, t, l, io, ls, to) = this.convertInternal(node)
        val t2 = t.map {
          case (k, v) =>
            k -> v.replace { s =>
              Some(if (l.contains(s)) v ∨ i else v)
            }
        }.toMap
        (DNF(DNF.symbol(s) ∨ i), t2, l | Set(s), io, ls, to)
      }
      case Plus(node) => {
        val (i, t, l, io, ls, to) = this.convertInternal(node)
        val t2 = t.map {
          case (k, v) =>
            k -> v.replace { s =>
              Some(if (l.contains(s)) v ∨ i else v)
            }
        }.toMap
        (i, t2, l, io, ls, to)
      }
      case Quest(node) => {
        val s = this.newSymbol()
        val (i, t, l, io, ls, to) = this.convertInternal(node)
        (DNF(DNF.symbol(s) ∨ i), t, l | Set(s), io, ls, to)
      }

      case Literal(char) => {
        val s1 = this.newSymbol()
        val s2 = this.newSymbol()
        (DNF(DNF.symbol(s1)),
         Map((s1, char) -> DNF(DNF.symbol(s2))),
         Set(s2),
         DNF.`0`,
         Set.empty,
         List.empty)
      }
      case Empty => {
        val s = this.newSymbol()
        (DNF(DNF.symbol(s)), Map.empty, Set(s), DNF.`0`, Set.empty, List.empty)
      }
    }
  }

}
