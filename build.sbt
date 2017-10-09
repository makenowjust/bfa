cancelable in Global := true // to abort current task by Ctrl-C

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.github.makenowjust.bfa",
      scalaVersion := "2.12.3",
      version := "0.1.0-SNAPSHOT",
      scalacOptions ++= Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
      )
    )),
  name := "bfa",
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  initialCommands in console := """
  import bfa._

  def a(p: String): Unit = {
    val node = Parser.parse(p).get
    println(s"AST  => ${node}")
    val mbfa = MBFA.from(node)
    println(s"MBFA => ${mbfa}")
    val dfa = DFA.from(mbfa)
    println(s"DFA  => ${dfa}")
    val node2 = dfa.toRegExp
    println(s"AST2 => ${node2}")
  }

  def m(p: String, s: String): Unit = {
    val node = Parser.parse(p).get
    println(s"AST  => ${node.matches(s)}")
    val mbfa = MBFA.from(node)
    println(s"MBFA => ${mbfa.matches(s)}")
    val dfa = DFA.from(mbfa)
    println(s"DFA  => ${dfa.matches(s)}")
    val node2 = dfa.toRegExp
    println(s"AST2 => ${node2.matches(s)}")
  }
  """
)
