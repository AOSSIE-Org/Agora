package org.aossie.agora.votecounter

import com.typesafe.scalalogging.LazyLogging
import spire.math.Rational

import org.aossie.agora.model._

import scala.collection.mutable.{HashMap => Map}

/** Created by deepeshpandey on 09/03/17. */
object BaldwinMethod extends VoteCounter[PreferenceBallot] with LazyLogging {

  def bordaScores[C <: Candidate](
      election: Election[C, PreferenceBallot],
      candidates: List[C]
  ): Map[C, Rational] = {
    val m = new Map[C, Rational]

    for (b <- election if !b.preferences.isEmpty) {
      // need to take the size of the list first and then calculate the borda scores
      var bordaCounter = candidates.length
      b.preferences
        .filter(candidate => candidates.contains(candidate))
        .map { candidate =>
          m(candidate) =
            m.getOrElse(candidate, Rational(0, 1)) + ((bordaCounter - 1) * b.weight.numerator.toInt)
          bordaCounter -= 1
        }
    }
    m
  }

  def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      candidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    logger.info("Baldwin rule method called")

    if (candidates.length == 1) {
      bordaScores(election, candidates).toList
    } else {
      // removing the lowest borda score candidate from the candidate list
      var lowestBordaCandidate = bordaScores(election, candidates).minBy(_._2)
      winners(election, candidates.filter(_ != lowestBordaCandidate._1), numVacancies)
    }
  }

}
