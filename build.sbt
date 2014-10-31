name := "Nidhoggr"

version := "1.0"

scalaVersion := "2.10.4"

resolvers ++= Seq(
"Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
"Akka Repo" at "http://repo.akka.io/snapshots"
)

fork in run := true

classpathTypes += "maven-plugin"

libraryDependencies ++= Seq(
  "io.spray" %% "spray-can" % "1.3.1",
  "io.spray" %% "spray-json" % "1.3.0",
  "io.spray" %% "spray-routing" % "1.3.1",
  "io.spray" %% "spray-http" % "1.3.1",
  "com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT",
  "com.sksamuel.scrimage" %% "scrimage" % "1.4.2",
  "com.sksamuel.scrimage" %% "scrimage-core" % "1.4.2" withSources(),
  "com.sksamuel.scrimage" %% "scrimage-filters" % "1.4.2"
)

shellPrompt in ThisBuild := { state => "sbt:" + Project.extract(state).currentRef.project + "> "}