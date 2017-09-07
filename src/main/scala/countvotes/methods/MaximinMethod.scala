package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._

import collection.mutable.{ListBuffer, HashMap => MMap}

/**
  * Algorithm : https://www.cs.cmu.edu/~arielpro/mfai_papers/lecture6.pdf page-4
  * Variant : winning votes => W = \arg \min_X ( \max_Y score(Y, X))
  */
object MaximinMethod extends VoteCountingMethod[WeightedBallot] with LazyLogging{

  private val result: Result = new Result
  private val report: Report[WeightedBallot] = new Report[WeightedBallot]
  private val rational0 = Rational(0, 1)
  private val majorityThreshold = Rational(1, 2)

  def runScrutiny(election: Election[WeightedBallot], candidates: List[Candidate], numVacancies: Int): Report[WeightedBallot] = {

    print("\n INPUT ELECTION: \n")
    printElection(election)

    report.setCandidates(candidates)

    report.setWinners(winners(election, candidates, numVacancies))

    report
  }

  def winners(election: Election[WeightedBallot], ccandidates: List[Candidate], numVacancies: Int):
  List[(Candidate, Rational)] = {

    logger.info("Computing maximin Condorcet Winner")

    val pairwiseComparisons = getPairwiseComparisonForWeightedElection(election, ccandidates)
    val mcScores = getMaximinScores(pairwiseComparisons, ccandidates, election).toList.sortWith(_._2>_._2)

    mcScores.head :: List()


  }

  def getMaximinScores(pairwiseComparisons: Array[Array[Rational]], ccandidates: List[Candidate], election: Election[WeightedBallot]): MMap[Candidate, Rational] = {

    val maximinScores = new MMap[Candidate, Rational]

    for(c <- ccandidates){
      maximinScores(c) = Rational(election.size, 1)
    }

    for (i <- ccandidates)
      for (j <- ccandidates)
        if (maximinScores(i) > pairwiseComparisons(ccandidates.indexOf(i))(ccandidates.indexOf(j)) & i != j) {
          maximinScores(i) = pairwiseComparisons(ccandidates.indexOf(i))(ccandidates.indexOf(j))
        }

    maximinScores
  }
}