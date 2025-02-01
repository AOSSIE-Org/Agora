package org.aossie.agora.votecounter

import org.aossie.agora.votecounter.BaldwinMethod.bordaScores
import org.aossie.agora.model._

import spire.math.Rational

/** https://en.wikipedia.org/wiki/Nanson%27s_method */
object Nanson extends VoteCounter[PreferenceBallot] {

  def winners[C <: Candidate](
      election: Election[C, PreferenceBallot],
      candidates: List[C],
      numVacancies: Int
  ): List[(C, Rational)] = {

    if (candidates.length == 1) {
      bordaScores(election, candidates).toList
    } else {
      var cbs     = bordaScores(election, candidates) // borda scores of candidates
      val average = candidates.foldLeft(Rational(0, 1))(_ + cbs(_)) / Rational(candidates.length)
      winners(election, candidates.filter(x => cbs(x) > average), numVacancies)
    }
  }

}
