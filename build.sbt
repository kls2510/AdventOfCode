ThisBuild / scalaVersion := "2.13.4"
ThisBuild / version      := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5",
                            "org.slf4j" % "slf4j-simple" % "1.7.5"
)

lazy val root = (project in file("src"))
  .settings(
    name := "AdventOfCode",
    Compile / scalaSource := baseDirectory.value / "src" / "scala",
    Compile / javaSource := baseDirectory.value / "src" / "java",
    // mainClass in (Compile, packageBin) := Some("RayTracing")
  )
