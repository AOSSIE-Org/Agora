import language.postfixOps

lazy val scala213 = "2.13.14"
lazy val scala304 = "3.4.2"
lazy val supportedScalaVersions = List(scala213, scala304)

ThisBuild / name := "agora"
ThisBuild / organization := "AOSSIE"
ThisBuild / version := "1.2"
ThisBuild / scalaVersion := scala304

resolvers += Resolver.sonatypeRepo("public")
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

logBuffered := false

parallelExecution in Test := false

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

scalacOptions in Test ++= Seq("-Yrangepos")

val allSettings = Defaults.coreDefaultSettings


lazy val root = Project("agora", file("."))
  .aggregate(
    core,
    cli
  ).settings(commonSettings, crossScalaVersions := Nil)

lazy val core = (project in file("modules/core"))
  .configs(Testing.configs *)
  .settings(Testing.settings *)
  .settings(
    commonSettings,
    crossScalaVersions := supportedScalaVersions,
    name    := "core"
  )

lazy val cli = (project in file("modules/cli"))
  .configs(Testing.configs *)
  .settings(Testing.settings *)
  .dependsOn(
    core
  )
  .settings(
    commonSettings,
    crossScalaVersions := supportedScalaVersions,
    name := "cli"
  )

lazy val commonSettings = Seq(
  scalafmtOnCompile := true,
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
//  scalacOptions += "-P:semanticdb:synthetics:on",
  scalacOptions += {
    if (scalaVersion.value.startsWith("2.12")) "-Ywarn-unused-import"
    else "-Wunused:imports"
  },


  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq(
        "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.0",
        "com.github.scopt" %% "scopt" % "4.1.0",
        "org.specs2" %% "specs2-core" % "4.20.6" % "test,verification-test,bench",
        "com.lihaoyi" %% "ammonite-ops" % "2.4.1",
        "ch.qos.logback" % "logback-classic" % "1.2.11",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
        "com.typesafe.play" %% "play-json" % "2.9.4",
        "org.typelevel" %% "spire" % "0.18.0"
      )
      case Some((3, 4)) => Seq(
        "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.0",
        "com.github.scopt" %% "scopt" % "4.1.0",
        "org.specs2" %% "specs2-core" % "4.20.6" % "test,verification-test,bench",
        ("com.lihaoyi" %% "ammonite-ops" % "2.4.1").cross(CrossVersion.for3Use2_13),
        "ch.qos.logback" % "logback-classic" % "1.2.11",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
        "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
        "com.typesafe.play" %% "play-json" % "2.10.6",
        "org.typelevel" %% "spire" % "0.18.0"
      )
      case _ => Nil
    }
  },

  dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-databind" % "2.9.0"
)

licenses := Seq("CC BY-NC-SA" -> url("http://creativecommons.org/licenses/by-nc-sa/4.0/"))

homepage := Some(url("https://www.gitlab.com/aossie/Agora"))

addCommandAlias("fix-lint", ";scalafixAll; scalafmtSbt;")