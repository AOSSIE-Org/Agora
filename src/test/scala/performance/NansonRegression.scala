package performance
import countvotes.methods.NansonRuleMethod
import countvotes.structures.WeightedBallot
import org.scalameter.api._

/**
  * Created by deepeshpandey on 13/05/17.
  */
trait NansonRegression extends AgoraBenchmark {

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

  override def persistor: Persistor = new GZIPJSONSerializationPersistor("target/benchmarks/nanson/time")

}

trait NansonMemoryRegression extends AgoraBenchmark {

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

  override def votingMethodName(): String = "Nanson"

  override def votingMethod(election: List[WeightedBallot]): Unit = {
    NansonRuleMethod.winners(election, randomPreference(), 1)
  }

  override def measurer = new Measurer.MemoryFootprint

  override def persistor: Persistor = new GZIPJSONSerializationPersistor("target/benchmarks/nanson/memory")

}

object NansonRegressionTest extends Bench.Group {

  //  perform regression for Nanson method
  performance of "memory" config(
    reports.resultDir -> "target/benchmarks/nanson/memory"
    ) in {
    include(new NansonMemoryRegression {})
  }

  performance of "running time" config(
    reports.resultDir -> "target/benchmarks/nanson/time"
    ) in {
    include(new NansonRegression {})
  }

}
