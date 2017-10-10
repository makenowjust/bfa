package bfa.example

import org.scalajs.dom
import dom.document

import bfa.{Parser, AST, MBFA, DFA}

object Main {
  def main(args: Array[String]): Unit = {
    val input = document.getElementById("input").asInstanceOf[dom.html.Input]
    val convert =
      document.getElementById("convert").asInstanceOf[dom.html.Button]
    val output = document.getElementById("output").asInstanceOf[dom.html.Div]

    convert.addEventListener(
      "click", { (_: dom.Event) =>
        val inText = input.value
        val inAST = Parser.parse(inText).getOrElse(AST.Empty)
        val mbfa = MBFA.from(inAST)
        val dfa = DFA.from(mbfa).minimize
        val outAST = dfa.toRegExp
        output.textContent = outAST.toString
      }
    )
  }
}
