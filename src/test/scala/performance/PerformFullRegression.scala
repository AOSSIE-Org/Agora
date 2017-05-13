package performance

import countvotes.structures.WeightedBallot
import org.scalameter.Bench
import org.scalameter.api._

/**
  * Created by deepeshpandey on 13/05/17.
  */
object PerformFullRegression extends Bench.Group {

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
