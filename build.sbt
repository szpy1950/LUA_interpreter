ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.7"

lazy val root = (project in file("."))
  .settings(
    name := "LUA_interpreter",
    Compile / run / mainClass := Some("Main"),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "4.0.2",
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )
  )