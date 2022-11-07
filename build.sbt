val dottyVersion = "3.2.1" // dottyLatestNightlyBuild.get

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

ThisBuild / githubWorkflowJavaVersions := Seq("8", "11", "17").map(JavaSpec.temurin)
ThisBuild / githubWorkflowScalaVersions := Seq(dottyVersion)
ThisBuild / githubWorkflowBuildPostamble := Seq(
  // This runs the template with the default parameters, and runs test within the templated app.
  WorkflowStep.Run(List("sbt test")),
)
ThisBuild / githubWorkflowPublishTargetBranches := Nil
Global / onChangedBuildSource := ReloadOnSourceChanges
