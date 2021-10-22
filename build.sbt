val dottyVersion = "3.1.0" // dottyLatestNightlyBuild.get

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
      "org.scala-lang.modules" % "scala-parser-combinators_2.13" % "1.1.2",
      "org.scala-lang.modules" % "scala-xml_2.13" % "1.2.0",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
