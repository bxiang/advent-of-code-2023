lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2023",
    organization := "com.aoc",
    scalaVersion := "3.3.1"
  )

val ZIOVersion = "2.0.19"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % ZIOVersion,
  "dev.zio" %% "zio-streams" % ZIOVersion,
  "dev.zio" %% "zio-test"   % ZIOVersion
)
