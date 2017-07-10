package performance

import countvotes.methods.{BordaRuleMethod, NansonRuleMethod, RandomBallotMethod}
import countvotes.structures.WeightedBallot
import org.scalameter.api._
import org.scalameter.persistence.GZIPJSONSerializationPersistor


trait RandomBallotRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "RandomBallot"

  override  def votingMethod(election: List[WeightedBallot]): Unit = {

    RandomBallotMethod.winners(election, randomPreference(), 1)
  }

}

trait RandomBallotMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "RandomBallot"

  override def votingMethod(election: List[WeightedBallot]): Unit = {
    RandomBallotMethod.winners(election, randomPreference(), 1)
  }

}

// existing issue : not overriding reports
object RandomBallotRegressionTest extends Bench.Group {
  //  perform regression for borda method
  performance of "memory" config(
    reports.resultDir -> "target/benchmarks/randomballot/memory"
    ) in {
    include(new RandomBallotMemoryRegression {})
  }

  performance of "running time" config(
    reports.resultDir -> "target/benchmarks/randomballot/time"
    ) in {
    include(new RandomBallotRegression {})
  }

}
