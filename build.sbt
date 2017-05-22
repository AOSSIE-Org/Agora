name := "countvotes"

organization := "AOSSIE"

version := "1.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("public")
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.github.scopt" %% "scopt" % "3.3.0",
  "org.specs2" %% "specs2-core" % "3.8.6" % "test",
  "com.lihaoyi" %% "ammonite-ops" % "0.8.1",
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "com.storm-enroute" %% "scalameter" % "0.8.2",
  "com.storm-enroute" %% "scalameter-core" % "0.8.2"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in Test := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize", "-Yinline-warnings")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

scalacOptions in Test ++= Seq("-Yrangepos")

val allSettings = Defaults.coreDefaultSettings


lazy val project = Project("agora", file("."), settings = allSettings)
  .configs(Testing.configs: _*)
  .settings(Testing.settings: _*)


licenses := Seq("CC BY-NC-SA" -> url("http://creativecommons.org/licenses/by-nc-sa/4.0/"))

homepage := Some(url("https://www.gitlab.com/aossie/Agora"))