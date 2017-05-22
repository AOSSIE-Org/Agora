import com.github.retronym.SbtOneJar._
import sbt.Keys._
import sbt._

import scala.language.postfixOps


object AgoraBuild extends Build {

  val allSettings = Defaults.coreDefaultSettings


  lazy val project = Project("project", file("."), settings = allSettings)
    .configs(Testing.configs: _*)
    .settings(Testing.settings: _*)
}
