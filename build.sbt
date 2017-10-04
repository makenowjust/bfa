lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.github.makenowjust.bfa",
      scalaVersion := "2.12.3",
      version := "0.1.0-SNAPSHOT"
    )),
  name := "bfa",
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % Test
)
