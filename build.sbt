val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "expressions-compiler",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.10" % "test",

    testFrameworks += new TestFramework("utest.runner.Framework"),
  )
