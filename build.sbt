val scala3Version = "3.1.1"



lazy val root = project
  .in(file("."))
  .settings(
    name := "expressions-processor",
    version := "0.1.1",

    scalaVersion := scala3Version,
    //scalacOptions := Seq("-Xcheck-macros"), //Cause errors with utest...

    assembly / assemblyOption ~= {
      _.withIncludeScala(false)
        .withIncludeDependency(false)
    },

    libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.10" % "test",

    testFrameworks += new TestFramework("utest.runner.Framework"),
    Test / parallelExecution := false
  )
