package countvotes.methods

import com.typesafe.scalalogging.LazyLogging
import countvotes.structures._
import countvotes.structures.{PreferenceBallot => Ballot}

import collection.mutable.{ListBuffer, HashMap => MMap}

/**
  * Algorithm : https://www.cs.cmu.edu/~arielpro/mfai_papers/lecture6.pdf page-4
  * Variant : winning votes => W = \arg \min_X ( \max_Y score(Y, X))
  */
object Maximin extends VoteCounter[Ballot] with LazyLogging{

  private val rational0 = Rational(0, 1)
  private val majorityThreshold = Rational(1, 2)


  def winners(election: Election[Ballot], ccandidates: List[Candidate], numVacancies: Int):
  List[(Candidate, Rational)] = {

    logger.info("Computing maximin Condorcet Winner")

    val pairwiseComparisons = getPairwiseComparisonForWeightedElection(election, ccandidates)
    val mcScores = getMaximinScores(pairwiseComparisons, ccandidates, election).toList.sortWith(_._2>_._2)

    mcScores.head :: List()


  }

  def getMaximinScores(pairwiseComparisons: Array[Array[Rational]], ccandidates: List[Candidate], election: Election[Ballot]): MMap[Candidate, Rational] = {

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