import sbtcrossproject.{crossProject, CrossType}

onLoad in Global ~= (_ andThen ("project rootJVM" :: _))

val commonSettings = List(
  organization := "com.github.makenowjust.bfa",
  scalaVersion := "2.12.3",
  version := "0.1.0-SNAPSHOT",
  cancelable := true // to abort current task by Ctrl-C
)

lazy val root = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(
    commonSettings,
    name := "bfa",
    // scala-parser-combinator v1.0.6 does not work on Scala.js, but v1.0.5 works fine.
    // See https://github.com/scala/scala-parser-combinators/issues/119
    libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test,
    initialCommands in console := """
    import bfa._

    def a(p: String): Unit = {
      val node = Parser.parse(p).get
      println(s"AST  => $node")
      val mbfa = MBFA.from(node)
      println(s"MBFA => $mbfa")
      val dfa = DFA.from(mbfa)
      println(s"DFA  => $dfa")
      val dfa2 = dfa.minimize
      println(s"DFA2 => $dfa2")
      val node2 = dfa.toRegExp
      println(s"AST2 => $node2")
    }

    def m(p: String, s: String): Unit = {
      val node = Parser.parse(p).get
      println(s"AST  => ${node.matches(s)}")
      val mbfa = MBFA.from(node)
      println(s"MBFA => ${mbfa.matches(s)}")
      val dfa = DFA.from(mbfa)
      println(s"DFA  => ${dfa.matches(s)}")
      val dfa2 = dfa.minimize
      println(s"DFA2 => ${dfa2.matches(s)}")
      val node2 = dfa.toRegExp
      println(s"AST2 => ${node2.matches(s)}")
    }
    """,
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
    ),
  )

lazy val rootJVM = root.jvm
lazy val rootJS = root.js

lazy val demo = project
  .dependsOn(rootJS)
  .enablePlugins(ScalaJSPlugin)
  .settings(
    commonSettings,
    name := "bfa-demo",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  )
