val dottyVersion = "0.7.0-bin-20180130-7058f69-NIGHTLY"

lazy val root = (project in file(".")).
  settings(
    name := "knock-dotty",
    version := "0.1.0",

    scalaVersion := dottyVersion
  )
