name := "grcanosa_1"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.scalamock" %% "scalamock" % "4.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
)