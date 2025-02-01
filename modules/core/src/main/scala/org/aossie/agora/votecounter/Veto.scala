package org.aossie.agora.votecounter

import org.aossie.agora.model._
import scala.collection.mutable.{HashMap => MMap}

object Veto extends VoteCounter[PreferenceBallot] {

  import spire.math.Rational

  def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      ccandidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {
    val candidateScoreMap = new MMap[C, Rational]
    for (c <- ccandidates) candidateScoreMap(c) = Rational(0, 1)

    for (ballot <- election) {
      for (preference <- ballot.preferences) {
        if (!(preference == ballot.preferences.last && ballot.preferences.length > 1)) {
          // Automatically assign a weight of 1 to all candidates in the ballot except the last candidate.
          // All ballot weights specified in the election file are ignored.
          candidateScoreMap(preference) =
            candidateScoreMap.getOrElse(preference, Rational(0, 1)) + Rational(1, 1)
        }
      }
    }

    candidateScoreMap.toList.sortWith(_._2 > _._2).take(numVacancies)
  }

}
