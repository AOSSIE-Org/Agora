import sbt.Keys._
import sbt._

object Testing {

  val VerificationTest = config("verification-test") extend Test
  val PerformanceTest = config("bench") extend Test

  val configs = Seq(VerificationTest, PerformanceTest)

  private lazy val VerificationTestSettings =
    inConfig(VerificationTest)(Defaults.testSettings) ++
    Seq(
      fork in VerificationTest := false,
      parallelExecution in VerificationTest := false,
      scalaSource in VerificationTest := baseDirectory.value / "src/test/scala/countvotes")

  private lazy val performanceTestSettings =
    inConfig(PerformanceTest)(Defaults.testSettings) ++
    Seq(
      fork in PerformanceTest := false,
      parallelExecution in PerformanceTest := false,
      scalaSource in PerformanceTest := baseDirectory.value / "src/test/scala/performance")
  

  lazy val testAllQuick = TaskKey[Unit]("test-all-quick")

  lazy val testAllQuickSettings = Seq(
    testAll := (),
    testAll := {(test in VerificationTest).value},
    testAll := {(test in VerificationTest).value},
    testAll := {(test in Test).value}
  )

  lazy val testAll = TaskKey[Unit]("test-all")

  lazy val testAllSettings = Seq(
    testAll := (),
    testAll := {(test in VerificationTest).value},
    testAll := {(test in PerformanceTest).value},
    testAll := {(test in Test).value}
  )

  lazy val settings = VerificationTestSettings ++
                      performanceTestSettings ++
                      testAllQuickSettings ++
                      testAllSettings
}
