name := "scala-json-traverse"

version := "3.0.4"

crossScalaVersions := Seq("2.11.7", "2.12.10")

libraryDependencies ++= Seq(
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.3.3",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)
