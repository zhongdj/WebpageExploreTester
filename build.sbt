name := "WebpageExploreTester"

version := "1.0"

scalaVersion := "2.10.2"

fork in run := true

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies += "org.scalaz.stream" %% "scalaz-stream" % "0.4.1"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.2.1",
  "com.typesafe.akka" %% "akka-testkit" % "2.2.1",
  "org.scalatest" %% "scalatest" % "1.9.2-SNAP2" % "test",
  "com.ning" % "async-http-client" % "1.8.13",
  "ch.qos.logback" % "logback-classic" % "1.0.7")

javaOptions += "-Dakka.loglevel=DEBUG"
