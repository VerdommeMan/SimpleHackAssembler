ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name := "SimpleHackAssembler",
    Compile / mainClass := Some("Main")
  )

libraryDependencies += "com.monovore" %% "decline" % "2.2.0"