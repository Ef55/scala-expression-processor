val scala3Version = "3.1.3-RC2"



lazy val root = project
  .in(file("."))
  .settings(
    name := "expressions-processor",
    version := "0.2.0",
    isSnapshot := true,

    scalaVersion := scala3Version,
    scalacOptions := Seq("-explain", "-feature"),

    assembly / assemblyOption ~= {
      _.withIncludeScala(false)
        .withIncludeDependency(false)
    },

    libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.10" % "test",

    testFrameworks += new TestFramework("utest.runner.Framework"),
    Test / parallelExecution := false
  )
