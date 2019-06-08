
ThisBuild / version      := "0.0.0"
ThisBuild / organization := "io.github.sherzodv"

val settings = Compiler.settings

lazy val `untyped` = project
  .settings(settings: _*)
  .settings(libraryDependencies ++= Dependencies.untyped)

lazy val root = (project in file("."))
  .settings(
    name := "Sequents"
  )
  .aggregate(
    `untyped`
  )
