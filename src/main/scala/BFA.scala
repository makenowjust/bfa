package bfa

final case class BFA(
    init: DNF.OrSet,
    trans: Map[(Symbol, Char), DNF.OrSet],
    last: Set[Symbol]
) {

  def matches(s: String): Boolean =
    s.foldLeft(init) { (e, c) =>
        println(e)
        e.replace { s =>
          trans.get((s, c))
        }
      }
      .evaluate(last)
}

object BFA {
  import AST._

  def from(node: AST): BFA =
    new Converter().convert(node)

  private final class Converter {

    type Result = (DNF.OrSet, Map[(Symbol, Char), DNF.OrSet], Set[Symbol])

    def convert(node: AST): BFA = {
      val (init, trans, last) = this.convertInternal(node)
      BFA(init, trans, last)
    }

    var symbolId = 0

    def newSymbol(): Symbol = {
      symbolId += 1
      Symbol(s"a$symbolId")
    }

    def convertInternal(node: AST): Result = node match {
      case Alt(left, right) => {
        val (i1, t1, l1) = this.convertInternal(left)
        val (i2, t2, l2) = this.convertInternal(right)
        (i1 ∨ i2, t1 ++ t2, l1 ++ l2)
      }

      case Concat(left, right) => {
        val (i1, t1, l1) = this.convertInternal(left)
        val (i2, t2, l2) = this.convertInternal(right)
        val i3 = i1.replace { s =>
          Some(if (l1.contains(s)) i2 else DNF(DNF.symbol(s)))
        }
        val t3 = t1.map {
          case (k, v) =>
            k -> v.replace { s =>
              Some(if (l1.contains(s)) i2 else v)
            }
        }.toMap ++ t2
        (i3, t3, l2)
      }

      case Star(node) => {
        val s = this.newSymbol()
        val (i, t, l) = this.convertInternal(node)
        val t2 = t.map {
          case (k, v) =>
            k -> v.replace { s =>
              Some(if (l.contains(s)) v ∨ i else v)
            }
        }.toMap
        (DNF(DNF.symbol(s) ∨ i), t2, l | Set(s))
      }
      case Plus(node) => {
        val (i, t, l) = this.convertInternal(node)
        val t2 = t.map {
          case (k, v) =>
            k -> v.replace { s =>
              Some(if (l.contains(s)) v ∨ i else v)
            }
        }.toMap
        (i, t2, l)
      }
      case Quest(node) => {
        val s = this.newSymbol()
        val (i, t, l) = this.convertInternal(node)
        (DNF(DNF.symbol(s) ∨ i), t, l | Set(s))
      }

      case Literal(char) => {
        val s1 = this.newSymbol()
        val s2 = this.newSymbol()
        (DNF(DNF.symbol(s1)), Map((s1, char) -> DNF(DNF.symbol(s2))), Set(s2))
      }
      case Empty => {
        val s = this.newSymbol()
        (DNF(DNF.symbol(s)), Map.empty, Set(s))
      }
    }
  }

}
