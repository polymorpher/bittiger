name := "bittiger"

version := "1.0"

scalaVersion := "2.11.8"
scalacOptions += "-optimise"
resolvers += Resolver.bintrayRepo("mskimm", "maven")
libraryDependencies ++= Seq(
  "com.fasterxml.jackson.module" % "jackson-module-scala_2.10" % "2.8.8",
  "org.clulab" %% "processors-corenlp" % "6.0.2",
  "org.clulab" %% "processors-main" % "6.0.2",
  "org.clulab" %% "processors-models" % "6.0.2",
  "cc.mallet" % "mallet" % "2.0.7-RC2",
  "com.tumblr" %% "colossus" % "0.8.3",
  "com.github.mskimm" %% "ann4s" % "0.0.6"
)