lazy val root = project
  .in(file("."))
  .settings(
    name         := "Advent of Code",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "3.3.4",
    scalacOptions := Seq(
      "-Wunused:all"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.1.0",
      "tools.aqua"              % "z3-turnkey"                 % "4.13.4",
      "org.scalatest"          %% "scalatest-flatspec"         % "3.2.19" % Test,
      "org.scalatest"          %% "scalatest-shouldmatchers"   % "3.2.19" % Test
    )
  )
