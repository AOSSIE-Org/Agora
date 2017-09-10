import language.postfixOps
import com.github.retronym.SbtOneJar._

name := "countvotes"

organization := "AOSSIE"

version := "1.2"

scalaVersion := "2.12.3"

resolvers += Resolver.sonatypeRepo("public")
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.github.scopt" %% "scopt" % "3.7.0",
  "org.specs2" %% "specs2-core" % "3.9.5" % "test,verification-test,bench",
  "com.lihaoyi" %% "ammonite-ops" % "1.0.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "com.storm-enroute" %% "scalameter" % "0.8.2",
  "com.storm-enroute" %% "scalameter-core" % "0.8.2",
  "com.typesafe.play" %% "play-json" % "2.6.3",
  "org.typelevel" %% "spire" % "0.14.1"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in Test := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

scalacOptions in Test ++= Seq("-Yrangepos")

val allSettings = Defaults.coreDefaultSettings ++ oneJarSettings


lazy val project = Project("agora", file("."), settings = allSettings)
  .configs(Testing.configs: _*)
  .settings(Testing.settings: _*)

mainClass in(oneJar) := Some("agora.Main")

licenses := Seq("CC BY-NC-SA" -> url("http://creativecommons.org/licenses/by-nc-sa/4.0/"))

homepage := Some(url("https://www.gitlab.com/aossie/Agora"))