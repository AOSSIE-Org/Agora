package performance

import countvotes.methods.BordaRuleMethod
import org.scalameter.api._
import countvotes.parsers.{CandidatesParser, PreferencesParser}
import countvotes.structures.{Candidate, WeightedBallot}

import scala.collection.immutable.ListSet
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
  * Created by deepeshpandey on 13/05/17.
  */
object BordaPerformanceTest extends Bench.LocalTime {

  val electionSizes: Gen[Int] = Gen.range("electionSize")(10000, 20000, 5000)

  val election: Gen[List[WeightedBallot]] = for {
    size <- electionSizes
  } yield getRandomElectionOfSize(size)

  performance of "VoteCountingMethod" in {
    measure method "Borda" in {
      using(election) in {
        preferences => BordaMethod(preferences)
      }
    }
  }

  def getRandomElectionOfSize(size: Int): List[WeightedBallot] = {

    val electionBuffer: ListBuffer[WeightedBallot] = new ListBuffer[WeightedBallot]

    for (i <- 1 to size) {
      electionBuffer.insert(i - 1, WeightedBallot(randomPreference(), i, 1))
    }

    electionBuffer.toList
  }

  def randomPreference(): List[Candidate] = {

    // generate random permutations uniformly for 5 candidates
    val preferenceSet: ListSet[Candidate] = ListSet(Candidate("A"), Candidate("B"), Candidate("C"), Candidate("D"), Candidate("E"))

    Random.shuffle(preferenceSet).toList

  }


  def BordaMethod(preferences: List[WeightedBallot]): Unit = {
    val candidates = randomPreference()
    BordaRuleMethod.winners(preferences, candidates, candidates.length)

  }


}
