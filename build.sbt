val dottyVersion = "0.15.0-bin-20190522-ffb250d-NIGHTLY" // dottyLatestNightlyBuild.get

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-xml-interpolator",
    version := "0.1.0",

    scalaVersion := dottyVersion,
    scalacOptions ++= Seq(
      "-Xprint-inline"
    ),
    libraryDependencies ++= Seq(
      ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1").withDottyCompat(scalaVersion.value),
      ("org.scala-lang.modules" %% "scala-xml" % "1.1.1").withDottyCompat(scalaVersion.value),
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
