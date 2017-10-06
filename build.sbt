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
        ))),
  name := "bfa",
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  initialCommands in console := """
  import bfa._

  def m(p: String, s: String): Unit = {
    val node = Parser.parse(p).get
    println(s"AST  => ${node.matches(s)}")
    println(s"MBFA => ${MBFA.from(node).matches(s)}")
  }
  """
)
