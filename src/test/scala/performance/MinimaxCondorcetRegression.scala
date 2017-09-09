package performance

import countvotes.methods.{MinimaxCondorcetMethod}
import countvotes.structures.Ballot
import org.scalameter.api._

/**
  * Created by deepeshpandey on 14/07/17.
  */
trait MinimaxCondorcetRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "MinimaxCondorcet"

  override  def votingMethod(election: List[Ballot]): Unit = {

    MinimaxCondorcetMethod.winners(election, randomPreference(), 1)
  }

}

trait MinimaxCondorcetMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "MinimaxCondorcet"

  override def votingMethod(election: List[Ballot]): Unit = {
    MinimaxCondorcetMethod.winners(election, randomPreference(), 1)
  }

}

object MinimaxCondorcetTest extends Bench.Group {

  performance of "memory" config(
    reports.resultDir -> "target/benchmarks/minimaxCondorcet/memory"
    ) in {
    include(new MinimaxCondorcetMemoryRegression {})
  }
  performance of "running time" config(
    reports.resultDir -> "target/benchmarks/minimaxCondorcet/time"
    ) in {
    include(new MinimaxCondorcetRegression {})
  }


}
