ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Git in Scala",
    libraryDependencies ++= Seq(
      "com.monovore" %% "decline" % "1.3.0"
    )
  ).enablePlugins(JavaAppPackaging)