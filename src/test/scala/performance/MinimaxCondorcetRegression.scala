package performance

import countvotes.methods.{MinimaxCondorcet}
import countvotes.structures.{Election}
import countvotes.structures.{PreferenceBallot => Ballot}
import org.scalameter.api._

/**
  * Created by deepeshpandey on 14/07/17.
  */
trait MinimaxCondorcetRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "MinimaxCondorcet"

  override  def votingMethod(election: Election[Ballot]): Unit = {

    MinimaxCondorcet.winners(election, randomPreference(), 1)
  }

}

trait MinimaxCondorcetMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "MinimaxCondorcet"

  override def votingMethod(election: Election[Ballot]): Unit = {
    MinimaxCondorcet.winners(election, randomPreference(), 1)
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
