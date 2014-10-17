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
  "io.spray" % "spray-can_2.10" % "1.3.1",
  "io.spray" % "spray-json_2.10" % "1.3.0",
  "io.spray" % "spray-routing_2.10" % "1.3.1",
  "io.spray" % "spray-http_2.10" % "1.3.1",
  "com.typesafe.akka" % "akka-actor_2.10" % "2.4-SNAPSHOT",
  "com.twelvemonkeys.imageio" % "imageio-tiff" % "3.0-rc7"
)

shellPrompt in ThisBuild := { state => "sbt:" + Project.extract(state).currentRef.project + "> "}