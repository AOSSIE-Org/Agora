package org.aossie.agora.votecounter

import org.aossie.agora.model._

import scala.collection.mutable.{HashMap => MMap}

import spire.math.Rational

/** Algorithm : https://en.wikipedia.org/wiki/Range_voting Note: This variant sums the score of
  * candidates over all voters TODO: Variant where average score of a candidate is used to compute
  * the winner
  */
object RangeVoting extends VoteCounter[ScoreBallot] {

  def winners[C <: Candidate](
      election: Election[C, ScoreBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    val candidateScores = new MMap[C, Rational]

    for (b <- election) {
      b.scores.foreach { case (candidate, score) =>
        candidateScores(candidate) =
          candidateScores.getOrElse(candidate, Rational(0, 1)) + score * b.weight
      }
    }
    // because range voting is a single seat election - numVacancies are always 1
    List(candidateScores.toList.maxBy(_._2))
  }

}
