import org.aossie.agora.votecounter.Copeland
import org.aossie.agora.model.{Election}
import org.aossie.agora.model.{PreferenceBallot => Ballot}
import org.scalameter.api.Bench
import org.scalameter.api._
import performance.BordaRegressionTest.include
import performance.{BordaMemoryRegression, BordaRegression, MemoryRegression, RuntimeRegression}

/**
  * Created by deepeshpandey on 27/06/17.
  */
trait CopelandRegression extends RuntimeRegression {

  override def votingMethodName(): String = "Copeland"

  override def votingMethod(election: Election[Ballot]): Unit = {
    Copeland.winners(election, randomPreference(), 1)
  }

}

trait CopelandMemoryRegression extends MemoryRegression {

  override def votingMethodName(): String = "Copeland"

  override def votingMethod(election: Election[Ballot]): Unit = {
    Copeland.winners(election, randomPreference(), 1)
  }

}

// existing issue : not overriding reports
object CopelandRegressionTest extends Bench.Group {
  //  perform regression for copeland method
  performance of "memory" config(
    reports.resultDir -> "target/benchmarks/copeland/memory"
    ) in {
    include(new CopelandMemoryRegression {})
  }

  performance of "running time" config(
    reports.resultDir -> "target/benchmarks/copeland/time"
    ) in {
    include(new CopelandRegression {})
  }

}

