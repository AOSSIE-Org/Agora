package performance

import countvotes.structures.Ballot
import org.scalameter.Bench
import org.scalameter.api._

/**
  * Created by deepeshpandey on 13/05/17.
  */
object FullRegression extends Bench.Group {

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
