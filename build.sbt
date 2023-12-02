lazy val root = project
  .in(file("."))
  .settings(
    name         := "Advent of code 2023",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "3.3.1",
    scalacOptions ++= Seq(
      "-no-indent"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest-flatspec"       % "3.2.17" % Test,
      "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.17" % Test
    )
  )
