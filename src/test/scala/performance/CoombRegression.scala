package performance

import countvotes.methods.{CoombRuleMethod}
import countvotes.structures.WeightedBallot
import org.scalameter.api._


trait CoombRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "Coomb"

  override  def votingMethod(election: List[WeightedBallot]): Unit = {

    CoombRuleMethod.winners(election, randomPreference(), 1)
  }

}

trait CoombMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "Coomb"

  override def votingMethod(election: List[WeightedBallot]): Unit = {
    CoombRuleMethod.winners(election, randomPreference(), 1)
  }

}

object CoombRegressionTest extends Bench.Group {

  //  perform regression for Coomb method
  performance of "memory" config(
    reports.resultDir -> "target/benchmarks/coomb/memory"
    ) in {
    include(new CoombMemoryRegression {})
  }

  performance of "running time" config(
    reports.resultDir -> "target/benchmarks/coomb/time"
    ) in {
    include(new CoombRegression {})
  }

}


