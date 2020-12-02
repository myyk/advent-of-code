import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.myyk",
      scalaVersion := "2.13.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "advent-of-code",
    libraryDependencies += scalaTest % Test,
    scalacOptions += "-deprecation"
)
