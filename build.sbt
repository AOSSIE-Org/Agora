import language.postfixOps

name := "countvotes"

organization := "AOSSIE"

version := "1.2"

scalaVersion := "2.12.19"

resolvers += Resolver.sonatypeRepo("public")
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.7",
  "com.github.scopt" %% "scopt" % "4.1.0",
  "org.specs2" %% "specs2-core" % "4.20.6" % "test,verification-test,bench",
  "com.lihaoyi" %% "ammonite-ops" % "2.4.1",
  "ch.qos.logback" % "logback-classic" % "1.2.11",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "com.storm-enroute" %% "scalameter" % "0.19",
  "com.storm-enroute" %% "scalameter-core" % "0.19",
  "com.typesafe.play" %% "play-json" % "2.9.4",
  "org.typelevel" %% "spire" % "0.17.0"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in Test := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

scalacOptions in Test ++= Seq("-Yrangepos")

val allSettings = Defaults.coreDefaultSettings


lazy val project = Project("agora", file("."))
  .configs(Testing.configs: _*)
  .settings(Testing.settings: _*)

licenses := Seq("CC BY-NC-SA" -> url("http://creativecommons.org/licenses/by-nc-sa/4.0/"))

homepage := Some(url("https://www.gitlab.com/aossie/Agora"))