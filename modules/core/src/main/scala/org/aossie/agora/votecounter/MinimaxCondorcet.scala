package org.aossie.agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import org.aossie.agora.model._
import org.aossie.agora.votecounter.common.PreferencePairwiseComparison

import spire.math.Rational

/** Algorithm : https://en.wikipedia.org/wiki/Minimax_Condorcet Variant : winning votes => W = \arg
  * \min_X ( \max_Y score(Y, X))
  */
object MinimaxCondorcet
    extends VoteCounter[PreferenceBallot]
    with PreferencePairwiseComparison[PreferenceBallot]
    with LazyLogging {

  private val rational0 = Rational(0, 1)

  private val majorityThreshold = Rational(1, 2)

  def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    logger.info("Computing minimax Condorcet Winner")

    val pairwiseComparisons = pairwiseComparison(election, ccandidates)
    val mcScores            = getMinimaxCondorcetScores(pairwiseComparisons, ccandidates)

    List(ccandidates.map(c => (c, mcScores.map(_(ccandidates.indexOf(c))).max)).minBy(_._2))

  }

  def getMinimaxCondorcetScores(
      pairwiseComparisons: Array[Array[Rational]],
      ccandidates: List[Candidate]
  ): Array[Array[Rational]] = {

    val minimaxCondorcetScores = Array.fill(ccandidates.size, ccandidates.size)(Rational(0, 1))

    for (i <- ccandidates.indices)
      for (j <- ccandidates.indices)
        if (pairwiseComparisons(i)(j) > pairwiseComparisons(j)(i)) {
          minimaxCondorcetScores(i)(j) = pairwiseComparisons(i)(j)
        }

    minimaxCondorcetScores
  }

}
