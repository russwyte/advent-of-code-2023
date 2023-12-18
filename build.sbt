val scala3Version = "3.3.1"

scalacOptions ++= Seq(
  "-feature"
)
lazy val root = project
  .in(file("."))
  .settings(
    name                                   := "Advent Of Code 2023",
    version                                := "0.1.0-SNAPSHOT",
    scalaVersion                           := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit"      % "1.0.0-M10" % Test,
    libraryDependencies += "dev.zio"       %% "zio-parser" % "0.1.9",
  )
