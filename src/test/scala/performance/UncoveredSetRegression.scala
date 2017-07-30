import countvotes.methods.UncoveredSetMethod
import countvotes.structures.WeightedBallot
import org.scalameter.api.Bench
import org.scalameter.api._
import performance.{MemoryRegression, RuntimeRegression}

/**
  * Regression test for computing uncovered set
  */
trait UncoveredSetRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "UncoveredSet"

  override  def votingMethod(election: List[WeightedBallot]): Unit = {

    UncoveredSetMethod.winners(election, randomPreference(), 1)
  }
}

trait UncoveredSetMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "UncoveredSet"

  override def votingMethod(election: List[WeightedBallot]): Unit = {
    UncoveredSetMethod.winners(election, randomPreference(), 1)
  }
}

object UncoveredSetRegressionTest extends Bench.Group {

  //  perform regression for Uncovered method
  performance of "memory" config(
    reports.resultDir -> "target/benchmarks/uncoveredset/memory"
    ) in {
    include(new UncoveredSetMemoryRegression {})
  }

  performance of "running time" config(
    reports.resultDir -> "target/benchmarks/uncoveredset/time"
    ) in {
    include(new UncoveredSetRegression {})
  }

}




