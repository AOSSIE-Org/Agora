package performance

import countvotes.methods.{Borda, BaldwinMethod}
import countvotes.structures.{Election}
import countvotes.structures.{PreferenceBallot => Ballot}
import org.scalameter.api._
import org.scalameter.persistence.GZIPJSONSerializationPersistor


trait BordaRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "Borda"

  override  def votingMethod(election: Election[Ballot]): Unit = {

    Borda.winners(election, randomPreference(), 1)
  }

}

trait BordaMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "Borda"

  override def votingMethod(election: Election[Ballot]): Unit = {
    Borda.winners(election, randomPreference(), 1)
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
