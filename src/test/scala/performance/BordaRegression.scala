package performance

import countvotes.methods.{BordaRuleMethod, NansonRuleMethod}
import countvotes.structures.WeightedBallot
import org.scalameter.api._
import org.scalameter.persistence.GZIPJSONSerializationPersistor

/**
  * Created by deepeshpandey on 13/05/17.
  */
trait BordaRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "Borda"

  override  def votingMethod(election: List[WeightedBallot]): Unit = {

    BordaRuleMethod.winners(election, randomPreference(), 1)
  }

}

trait BordaMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "Borda"

  override def votingMethod(election: List[WeightedBallot]): Unit = {
    BordaRuleMethod.winners(election, randomPreference(), 1)
  }

}

// existing issue : not overriding reports
object BordaRegressionTest extends Bench.Group {
  //  perform regression for borda method
  performance of "memory" config(
    reports.resultDir -> "target/benchmarks/borda/memory"
    ) in {
    include(new BordaMemoryRegression {})
  }

  performance of "running time" config(
    reports.resultDir -> "target/benchmarks/borda/time"
    ) in {
    include(new BordaRegression {})
  }

}
