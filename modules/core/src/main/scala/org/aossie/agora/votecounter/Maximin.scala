package org.aossie.agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import org.aossie.agora.model._
import org.aossie.agora.votecounter.common.PreferencePairwiseComparison

import collection.mutable.{HashMap => MMap}

import spire.math.Rational

/** Algorithm : https://www.cs.cmu.edu/~arielpro/mfai_papers/lecture6.pdf page-4 Variant : winning
  * votes => W = \arg \min_X ( \max_Y score(Y, X))
  */
object Maximin
    extends VoteCounter[PreferenceBallot]
    with PreferencePairwiseComparison
    with LazyLogging {

  private val rational0 = Rational(0, 1)

  private val majorityThreshold = Rational(1, 2)

  def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    logger.info("Computing maximin Condorcet Winner")

    val pairwiseComparisons = pairwiseComparison(election, ccandidates)
    val mcScores =
      getMaximinScores(pairwiseComparisons, ccandidates, election).toList.sortWith(_._2 > _._2)

    mcScores.head :: List()

  }

  def getMaximinScores[C <: Candidate](
      pairwiseComparisons: Array[Array[Rational]],
      ccandidates: List[C],
      election: Election[C, PreferenceBallot]
  ): MMap[C, Rational] = {

    val maximinScores = new MMap[C, Rational]

    for (c <- ccandidates)
      maximinScores(c) = Rational(election.size, 1)

    for (i <- ccandidates)
      for (j <- ccandidates)
        if (
          maximinScores(i) > pairwiseComparisons(ccandidates.indexOf(i))(
            ccandidates.indexOf(j)
          ) & i != j
        ) {
          maximinScores(i) = pairwiseComparisons(ccandidates.indexOf(i))(ccandidates.indexOf(j))
        }

    maximinScores
  }

}
