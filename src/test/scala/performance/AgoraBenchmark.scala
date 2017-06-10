package performance

import countvotes.methods.BordaRuleMethod
import countvotes.structures.{Candidate, WeightedBallot}
import org.scalameter.Bench
import org.scalameter.api._
import org.scalameter.persistence.GZIPJSONSerializationPersistor

import scala.collection.immutable.ListSet
import scala.util.Random

/**
  * Created by deepeshpandey on 13/05/17.
  */
class AgoraBenchmark extends Bench.OfflineRegressionReport {

  val preferenceSet: ListSet[Candidate] = ListSet(Candidate("A"), Candidate("B"), Candidate("C"), Candidate("D"), Candidate("E"))
  val electionSizes: Gen[Int] = Gen.range("electionSize")(10000, 20000, 5000)

  val election = for {
    size <- electionSizes
  } yield {
    for {
      i <- List.range(1, size)
    } yield WeightedBallot(randomPreference(), i, 1)
  }

  def randomPreference(): List[Candidate] = {

    // generate random permutations uniformly for 5 candidates
    Random.shuffle(preferenceSet).toList
  }

  def votingMethodName(): String = {
    ""
  }

  def votingMethod(election: List[WeightedBallot]) = {}
}


trait RuntimeRegression extends AgoraBenchmark {
  override def persistor: Persistor = new GZIPJSONSerializationPersistor("target/benchmarks/borda/time")

  performance of "VotingMethod" in {
    measure method votingMethodName() config (
      exec.benchRuns -> 15
      ) in {
      using(election) in {
        preferences => votingMethod(preferences)
      }
    }
  }

  def votingMethodName(): String

  def votingMethod(election: List[WeightedBallot]): Unit
}

trait MemoryRegression extends AgoraBenchmark {
  override def measurer = new Measurer.MemoryFootprint

  override def persistor: Persistor = new GZIPJSONSerializationPersistor("target/benchmarks/borda/memory")

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

  def votingMethodName(): String

  def votingMethod(election: List[WeightedBallot]): Unit
}




