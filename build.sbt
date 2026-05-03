ThisBuild / scalaVersion := "3.8.3"

lazy val root = (project in file("."))
  .settings(
    name := "REP",
    libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-RC1"
  )
