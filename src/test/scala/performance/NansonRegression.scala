package performance
import countvotes.methods.NansonRuleMethod
import countvotes.structures.WeightedBallot
import org.scalameter.api._

/**
  * Created by deepeshpandey on 13/05/17.
  */
trait NansonRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "Nanson"

  override  def votingMethod(election: List[WeightedBallot]): Unit = {

    NansonRuleMethod.winners(election, randomPreference(), 1)
  }

}

trait NansonMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "Nanson"

  override def votingMethod(election: List[WeightedBallot]): Unit = {
    NansonRuleMethod.winners(election, randomPreference(), 1)
  }

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
