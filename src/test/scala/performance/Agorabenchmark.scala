package performance

import countvotes.methods.NansonRuleMethod
import countvotes.structures.{Candidate, WeightedBallot}
import org.scalameter.Bench
import org.scalameter.api._

import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by deepeshpandey on 13/05/17.
  */
class Agorabenchmark extends Bench.OfflineRegressionReport {

  val preferenceSet: ListSet[Candidate] = ListSet(Candidate("A"), Candidate("B"), Candidate("C"), Candidate("D"), Candidate("E"))
  val electionSizes: Gen[Int] = Gen.range("electionSize")(10000, 20000, 5000)

  val election: Gen[List[WeightedBallot]] = for {
    size <- electionSizes
  } yield getRandomElectionOfSize(size)

  def getRandomElectionOfSize(size: Int): List[WeightedBallot] = {

    val electionBuffer: ListBuffer[WeightedBallot] = new ListBuffer[WeightedBallot]

    for (i <- 1 to size) {
      electionBuffer.insert(i - 1, WeightedBallot(randomPreference(), i, 1))
    }

    electionBuffer.toList
  }

  def randomPreference(): List[Candidate] = {

    // generate random permutations uniformly for 5 candidates
    Random.shuffle(preferenceSet).toList
  }

  def votingMethodName(): String = {""}

  def votingMethod(election: List[WeightedBallot]) = {}
}




