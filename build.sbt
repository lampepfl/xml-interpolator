val dottyVersion = "0.12.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-xml-interpolator",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1").withDottyCompat(scalaVersion.value),
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
