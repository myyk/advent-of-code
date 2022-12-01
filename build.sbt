import Dependencies._
import com.typesafe.sbt.packager.docker._

ThisBuild / scalaVersion     := "3.2.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "myyk"

lazy val root = (project in file("."))
  // .enablePlugins(JavaAppPackaging)
  .settings(
    licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    libraryDependencies ++= Seq(
      catsCore,
      scalaTest % Test
    )
  )
