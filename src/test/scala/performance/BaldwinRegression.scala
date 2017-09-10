package performance
import countvotes.methods.BaldwinMethod
import countvotes.structures.{Ballot, Election}
import org.scalameter.api._

/**
  * Created by deepeshpandey on 13/05/17.
  */
trait BaldwinRegression extends RuntimeRegression {

  override  def votingMethodName(): String = "Baldwin"

  override  def votingMethod(election: Election[Ballot]): Unit = {

    BaldwinMethod.winners(election, randomPreference(), 1)
  }

}

trait BaldwinMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "Baldwin"

  override def votingMethod(election: Election[Ballot]): Unit = {
    BaldwinMethod.winners(election, randomPreference(), 1)
  }

}

object BaldwinRegressionTest extends Bench.Group {

  //  perform regression for Baldwin method
  performance of "memory" config(
    reports.resultDir -> "target/benchmarks/baldwin/memory"
    ) in {
    include(new BaldwinMemoryRegression {})
  }

  performance of "running time" config(
    reports.resultDir -> "target/benchmarks/baldwin/time"
    ) in {
    include(new BaldwinRegression {})
  }

}
