package performance

import countvotes.methods.RandomBallot
import countvotes.structures.{Ballot, Election}
import org.scalameter.api._


trait RandomBallotRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "RandomBallot"

  override  def votingMethod(election: Election[Ballot]): Unit = {

    RandomBallot.winners(election, randomPreference(), 1)
  }

}

trait RandomBallotMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "RandomBallot"

  override def votingMethod(election: Election[Ballot]): Unit = {
    RandomBallot.winners(election, randomPreference(), 1)
  }

}

// existing issue : not overriding reports
object RandomBallotRegressionTest extends Bench.Group {
  //  perform regression for Random Ballot method
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
