package performance

import countvotes.structures.{Candidate, WeightedBallot}
import org.scalameter.Bench
import org.scalameter.api._

import scala.collection.immutable.ListSet
import scala.util.Random

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




