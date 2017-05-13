package performance

import countvotes.methods.{BordaRuleMethod, NansonRuleMethod}
import countvotes.structures.WeightedBallot
import org.scalameter.api._
import org.scalameter.persistence.GZIPJSONSerializationPersistor

/**
  * Created by deepeshpandey on 13/05/17.
  */
trait BordaRegression extends Agorabenchmark {

  override def persistor: Persistor = new GZIPJSONSerializationPersistor("target/benchmarks/borda/time")

  performance of "VotingMethod" in {
    measure method votingMethodName() config (
      exec.benchRuns -> 15
      ) in {
      using(election) in {
        preferences => votingMethod(preferences)
      }
    }
  }

  override  def votingMethodName(): String = "Nanson"

  override  def votingMethod(election: List[WeightedBallot]): Unit = {

    NansonRuleMethod.winners(election, randomPreference(), 1)
  }

}

trait BordaMemoryRegression extends Agorabenchmark {

  override def measurer = new Measurer.MemoryFootprint

  override def persistor: Persistor = new GZIPJSONSerializationPersistor("target/benchmarks/borda/memory")

  performance of "MemoryFootprint" in {
    performance of votingMethodName() in {
      using(election) config(
        exec.minWarmupRuns -> 2,
        exec.maxWarmupRuns -> 5,
        exec.benchRuns -> 5,
        exec.independentSamples -> 1
        ) in {
        preferences => votingMethod(preferences)
      }
    }
  }

  override def votingMethodName(): String = "Borda"

  override def votingMethod(election: List[WeightedBallot]): Unit = {
    BordaRuleMethod.winners(election, randomPreference(), 1)
  }

}

object BordaRegressionTest extends Bench.Group {
  //  perform regression for borda method
  performance of "memory" config(
    reports.resultDir -> "target/benchmarks/nanson/memory"
    ) in {
    include(new BordaMemoryRegression {})
  }

  performance of "running time" config(
    reports.resultDir -> "target/benchmarks/nanson/time"
    ) in {
    include(new BordaRegression {})
  }

}
