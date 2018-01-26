name := "knockknock"

version := "0.1"

scalaOrganization := "org.typelevel"

scalaVersion := "2.12.4-bin-typelevel-4"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.2"
libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value


scalacOptions ++= Seq(
  "-Yliteral-types",
  "-Ypartial-unification"
)

