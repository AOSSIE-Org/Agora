name := "countvotes"

organization := "AOSSIE"

version := "1.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "com.github.scopt" %% "scopt" % "3.3.0",
  "org.specs2" %% "specs2-core" % "3.8.6" % "test",
  "com.storm-enroute" %% "scalameter" % "0.7",
  "com.lihaoyi" %% "ammonite-ops" % "0.8.1"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
  
logBuffered := false


scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-optimize", "-Yinline-warnings")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

scalacOptions in Test ++= Seq("-Yrangepos")

licenses := Seq("CC BY-NC-SA" -> url("http://creativecommons.org/licenses/by-nc-sa/4.0/"))

homepage := Some(url("https://www.gitlab.com/aossie/Agora"))