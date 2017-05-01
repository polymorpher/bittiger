name := "bittiger"

version := "1.0"

scalaVersion := "2.11.8"
scalacOptions += "-optimise"
libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" % "jackson-module-scala_2.10" % "2.8.8",
  "org.clulab" %% "processors-corenlp" % "6.0.2",
  "org.clulab" %% "processors-main" % "6.0.2",
  "org.clulab" %% "processors-models" % "6.0.2"
)